/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers

import config.featureSwitches.FeatureSwitching
import connectors.getFinancialDetails.GetFinancialDetailsConnector
import connectors.getPenaltyDetails.GetPenaltyDetailsConnector
import connectors.parsers.getFinancialDetails.GetFinancialDetailsParser
import connectors.parsers.getFinancialDetails.GetFinancialDetailsParser.GetFinancialDetailsSuccessResponse
import connectors.parsers.getPenaltyDetails.GetPenaltyDetailsParser
import connectors.parsers.getPenaltyDetails.GetPenaltyDetailsParser.GetPenaltyDetailsSuccessResponse
import models.EnrolmentKey
import models.api.APIModel
import models.auditing.{ThirdParty1812APIRetrievalAuditModel, ThirdPartyAPI1811RetrievalAuditModel, UserHasPenaltyAuditModel}
import models.getFinancialDetails.FinancialDetails
import models.getPenaltyDetails.GetPenaltyDetails
import play.api.Configuration
import play.api.libs.json.{JsString, JsValue, Json}
import play.api.mvc._
import services.auditing.AuditService
import services.{APIService, FilterService, GetFinancialDetailsService, GetPenaltyDetailsService}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import utils.Logger.logger
import utils.PagerDutyHelper.PagerDutyKeys._
import utils.{DateHelper, PagerDutyHelper}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class APIController @Inject()(auditService: AuditService,
                              apiService: APIService,
                              getPenaltyDetailsService: GetPenaltyDetailsService,
                              getFinancialDetailsService: GetFinancialDetailsService,
                              getFinancialDetailsConnector: GetFinancialDetailsConnector,
                              getPenaltyDetailsConnector: GetPenaltyDetailsConnector,
                              dateHelper: DateHelper,
                              cc: ControllerComponents,
                              filterService: FilterService)(implicit ec: ExecutionContext, val config: Configuration) extends BackendController(cc) with FeatureSwitching {

  def getSummaryData(regime: String, id: String): Action[AnyContent] = Action.async {
    implicit request => {
      composeEnrolmentKey(regime, id).andThen { enrolmentKey =>
        import enrolmentKey._
        getPenaltyDetailsService.getDataFromPenaltyService(enrolmentKey).flatMap {
          _.fold({
            case GetPenaltyDetailsParser.GetPenaltyDetailsFailureResponse(status) if status == NOT_FOUND => {
              //logger.info(s"[APIController][getSummaryDataForVRN] - 1812 call (VATVC/BTA API) returned $status for VRN: $vrn")
              Future(NotFound(s"A downstream call returned 404 for $keyType: $key"))
            }
            case GetPenaltyDetailsParser.GetPenaltyDetailsFailureResponse(status) if status == UNPROCESSABLE_ENTITY => {
              //Temporary measure to avoid 422 causing issues
              val responsePayload = GetPenaltyDetailsSuccessResponse(GetPenaltyDetails(totalisations = None, lateSubmissionPenalty = None, latePaymentPenalty = None, breathingSpace = None))
              //logger.info(s"[APIController][getSummaryDataForVRN] - 1812 call (VATVC/BTA API) returned $status for VRN: $vrn - Overriding response")
              Future(returnResponseForAPI(responsePayload.penaltyDetails, enrolmentKey))
            }
            case GetPenaltyDetailsParser.GetPenaltyDetailsFailureResponse(status) => {
              //logger.info(s"[APIController][getSummaryDataForVRN] - 1812 call (VATVC/BTA API) returned an unexpected status: $status")
              Future(InternalServerError(s"A downstream call returned an unexpected status: $status for $info"))
            }
            case GetPenaltyDetailsParser.GetPenaltyDetailsMalformed => {
              PagerDutyHelper.log("getSummaryDataForVRN", MALFORMED_RESPONSE_FROM_1812_API)
              //logger.error(s"[APIController][getSummaryDataForVRN] - 1812 call (VATVC/BTA API) returned invalid body - failed to parse penalty details response for VRN: $vrn")
              Future(InternalServerError(s"We were unable to parse penalty data."))
            }
            case GetPenaltyDetailsParser.GetPenaltyDetailsNoContent => {
              logger.info(s"[APIController][getSummaryDataForVRN] - 1812 call (VATVC/BTA API) returned no content for $info")
              Future(NoContent)
            }
          },
            success => {
              logger.info(s"[APIController][getSummaryDataForVRN] - 1812 call (VATVC/BTA API) returned 200 for $info")
              val penaltyDetails = success.asInstanceOf[GetPenaltyDetailsSuccessResponse].penaltyDetails
              if (penaltyDetails.latePaymentPenalty.exists(LPP =>
                LPP.ManualLPPIndicator.getOrElse(false))) {
                logger.info(s"[APIController][getSummaryDataForVRN] - 1812 data has ManualLPPIndicator set to true, calling 1811")
                callFinancialDetailsForManualLPPs(enrolmentKey).map {
                  financialDetails => {
                    returnResponseForAPI(penaltyDetails, enrolmentKey, financialDetails)
                  }
                }
              } else {
                Future(returnResponseForAPI(penaltyDetails, enrolmentKey))
              }
            }
          )
        }
      }
    }
  }

  private def callFinancialDetailsForManualLPPs(enrolmentKey: EnrolmentKey)(implicit hc: HeaderCarrier): Future[Option[FinancialDetails]] = {
    getFinancialDetailsService.getFinancialDetails(enrolmentKey, None).map {
      financialDetailsResponseWithoutClearedItems =>
        logger.info(s"[APIController][callFinancialDetailsForManualLPPs] - Calling 1811 for response without cleared items")
        financialDetailsResponseWithoutClearedItems.fold({
          case GetFinancialDetailsParser.GetFinancialDetailsFailureResponse(status) =>
            logger.info(s"[APIController][callFinancialDetailsForManualLPPs] - 1811 call (VATVC/BTA API)" +
              s" returned an unexpected status: $status, returning None")
            None
          case GetFinancialDetailsParser.GetFinancialDetailsMalformed =>
            PagerDutyHelper.log("callFinancialDetailsForManualLPPs", MALFORMED_RESPONSE_FROM_1811_API)
            logger.error(s"[APIController][callFinancialDetailsForManualLPPs] - 1811 call (VATVC/BTA API)" +
              s" returned invalid body - failed to parse penalty details response for ${enrolmentKey.info}, returning None")
            None
          case GetFinancialDetailsParser.GetFinancialDetailsNoContent =>
            logger.info(s"[APIController][callFinancialDetailsForManualLPPs] - 1811 call (VATVC/BTA API) returned no content for ${enrolmentKey.info}, returning None")
            None
        },
          financialDetailsResponseWithoutClearedItems => {
            logger.info(s"[APIController][callFinancialDetailsForManualLPPs] - 1811 call (VATVC/BTA API) returned 200 for ${enrolmentKey.info}" )
            Some(financialDetailsResponseWithoutClearedItems.asInstanceOf[GetFinancialDetailsSuccessResponse].financialDetails)
          })
    }
  }

  private def returnResponseForAPI(penaltyDetails: GetPenaltyDetails, enrolmentKey: EnrolmentKey,
                                   financialDetails: Option[FinancialDetails] = None)(implicit request: Request[_]): Result = {
    val pointsTotal = penaltyDetails.lateSubmissionPenalty.map(_.summary.activePenaltyPoints).getOrElse(0)
    val penaltyAmountWithEstimateStatus = apiService.findEstimatedPenaltiesAmount(penaltyDetails)
    val noOfEstimatedPenalties = apiService.getNumberOfEstimatedPenalties(penaltyDetails)
    val crystallisedPenaltyAmount = apiService.getNumberOfCrystallisedPenalties(penaltyDetails, financialDetails)
    val crystallisedPenaltyTotal = apiService.getCrystallisedPenaltyTotal(penaltyDetails, financialDetails)
    val hasAnyPenaltyData = apiService.checkIfHasAnyPenaltyData(penaltyDetails)
    val responseData: APIModel = APIModel(
      noOfPoints = pointsTotal,
      noOfEstimatedPenalties = noOfEstimatedPenalties,
      noOfCrystalisedPenalties = crystallisedPenaltyAmount,
      estimatedPenaltyAmount = penaltyAmountWithEstimateStatus,
      crystalisedPenaltyAmountDue = crystallisedPenaltyTotal,
      hasAnyPenaltyData = hasAnyPenaltyData
    )
    if (hasAnyPenaltyData) {
      val auditModel = UserHasPenaltyAuditModel(
        penaltyDetails = penaltyDetails,
        enrolmentKey = enrolmentKey,
        arn = None,
        dateHelper = dateHelper)
      auditService.audit(auditModel)
      Ok(Json.toJson(responseData))
    } else {
      logger.info("[APIController][returnResponseForAPI] - User had no penalty data, returning 204 to caller")
      NoContent
    }
  }

  def getFinancialDetails(regime: String, idType: String, id: String,
                          searchType: Option[String],
                          searchItem: Option[String],
                          dateType: Option[String],
                          dateFrom: Option[String],
                          dateTo: Option[String],
                          includeClearedItems: Option[Boolean],
                          includeStatisticalItems: Option[Boolean],
                          includePaymentOnAccount: Option[Boolean],
                          addRegimeTotalisation: Option[Boolean],
                          addLockInformation: Option[Boolean],
                          addPenaltyDetails: Option[Boolean],
                          addPostedInterestDetails: Option[Boolean],
                          addAccruingInterestDetails: Option[Boolean]): Action[AnyContent] = Action.async {
    implicit request => {
      composeEnrolmentKey(regime, idType, id).andThen { enrolmentKey =>
        val response = getFinancialDetailsConnector.getFinancialDetailsForAPI(enrolmentKey,
          searchType,
          searchItem,
          dateType,
          dateFrom,
          dateTo,
          includeClearedItems,
          includeStatisticalItems,
          includePaymentOnAccount,
          addRegimeTotalisation,
          addLockInformation,
          addPenaltyDetails,
          addPostedInterestDetails,
          addAccruingInterestDetails
        )

        response.map(
          res => {
            val auditToSend = ThirdPartyAPI1811RetrievalAuditModel(enrolmentKey, res.status, res.body)
            auditService.audit(auditToSend)
            res.status match {
              case OK =>
                logger.info(s"[APIController][getFinancialDetails] - 1811 call (3rd party API) returned 200 for ${enrolmentKey.info}")
                logger.debug("[APIController][getFinancialDetails] Ok response received: " + res)
                Ok(res.json)
              case NOT_FOUND =>
                logger.error("[APIController][getFinancialDetails] - 1811 call (3rd party API) returned 404 - error received: " + res)
                Status(res.status)(Json.toJson(res.body))
              case status =>
                PagerDutyHelper.logStatusCode("getFinancialDetails", status)(RECEIVED_4XX_FROM_1811_API, RECEIVED_5XX_FROM_1811_API)
                logger.error(s"[APIController][getFinancialDetails] - 1811 call (3rd party API) returned an unknown error - status ${res.status} returned from EIS")
                Status(res.status)(Json.toJson(res.body))
            }
          })
      }
    }
  }

  def getPenaltyDetails(regime: String, idType: String, id: String, dateLimit: Option[String]): Action[AnyContent] = Action.async {
    implicit request => {
      composeEnrolmentKey(regime, idType, id).andThen { enrolmentKey =>
        val response = getPenaltyDetailsConnector.getPenaltyDetailsForAPI(enrolmentKey, dateLimit)
        response.map(
          res => {
            val processedResBody = filterService.tryJsonParseOrJsString(res.body)
            val filteredResBody = if (res.status.equals(OK) || !processedResBody.isInstanceOf[JsString]) {
              filterResponseBody(
                processedResBody, enrolmentKey, "getPenaltyDetails")
            } else {
              processedResBody
            }
            val auditToSend = ThirdParty1812APIRetrievalAuditModel(enrolmentKey, res.status, filteredResBody)
            auditService.audit(auditToSend)
            res.status match {
              case OK =>
                logger.info(s"[APIController][getPenaltyDetails] - 1812 call (3rd party API) returned 200 for ${enrolmentKey.info}")
                logger.debug("[APIController][getPenaltyDetails] Ok response received: " + res)
                Ok(filteredResBody)
              case NOT_FOUND =>
                logger.error("[APIController][getPenaltyDetails] - 1812 call (3rd party API) returned 404 - error received: " + res)
                Status(res.status)(Json.toJson(res.body))
              case status =>
                PagerDutyHelper.logStatusCode("getPenaltyDetails", status)(RECEIVED_4XX_FROM_1812_API, RECEIVED_5XX_FROM_1812_API)
                logger.error(s"[APIController][getPenaltyDetails] - 1812 call (3rd party API) returned an unknown error - status ${res.status} returned from EIS")
                Status(res.status)(Json.toJson(res.body))
            }
          }
        )
      }
    }
  }

  private def filterResponseBody(resBody: JsValue, enrolmentKey: EnrolmentKey, method: String): JsValue = {
    val penaltiesDetails = GetPenaltyDetails.format.reads(resBody)
    GetPenaltyDetails.format.writes(filterService.filterEstimatedLPP1DuringPeriodOfFamiliarisation(
      filterService.filterPenaltiesWith9xAppealStatus(penaltiesDetails.get)("APIConnector", method, enrolmentKey), "APIConnector", method, enrolmentKey))
  }
}
