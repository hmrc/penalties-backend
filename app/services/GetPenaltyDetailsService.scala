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

package services

import config.featureSwitches.FeatureSwitching
import connectors.getPenaltyDetails.GetPenaltyDetailsConnector
import connectors.parsers.getPenaltyDetails.GetPenaltyDetailsParser._
import models.EnrolmentKey
import play.api.Configuration
import uk.gov.hmrc.http.HeaderCarrier
import utils.Logger.logger

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class GetPenaltyDetailsService @Inject()(getPenaltyDetailsConnector: GetPenaltyDetailsConnector,
                                         filterService: FilterService)
                                        (implicit ec: ExecutionContext, val config: Configuration) extends FeatureSwitching {

  def getDataFromPenaltyService(enrolmentKey: EnrolmentKey)(implicit hc: HeaderCarrier): Future[GetPenaltyDetailsResponse] = {
    val startOfLogMsg: String = s"[GetPenaltyDetailsService][getDataFromPenaltyService][${enrolmentKey.regime}]"
    getPenaltyDetailsConnector.getPenaltyDetails(enrolmentKey).map {
      handleConnectorResponse(_)(startOfLogMsg, enrolmentKey)
    }
  }

  private def handleConnectorResponse(connectorResponse: GetPenaltyDetailsResponse)
                                     (implicit startOfLogMsg: String, enrolmentKey: EnrolmentKey): GetPenaltyDetailsResponse = {
    connectorResponse match {
      case res@Right(_@GetPenaltyDetailsSuccessResponse(penaltyDetails)) =>
        val callingClass: String = "GetPenaltiesDetailsService"
        val function: String = "handleConnectorResponse"
        logger.debug(s"$startOfLogMsg - Got a success response from the connector. Parsed model: $penaltyDetails")
        Right(GetPenaltyDetailsSuccessResponse(filterService.filterEstimatedLPP1DuringPeriodOfFamiliarisation(
          filterService.filterPenaltiesWith9xAppealStatus(penaltyDetails)(callingClass, function, enrolmentKey), callingClass, function, enrolmentKey)
        ))
      case res@Left(GetPenaltyDetailsNoContent) =>
        logger.debug(s"$startOfLogMsg - Got a 404 response and no data was found for GetPenaltyDetails call")
        res
      case res@Left(GetPenaltyDetailsMalformed) =>
        logger.info(s"$startOfLogMsg - Failed to parse HTTP response into model for ${enrolmentKey.keyType}: ${enrolmentKey.key}")
        res
      case res@Left(GetPenaltyDetailsFailureResponse(_)) =>
        logger.error(s"$startOfLogMsg - Unknown status returned from connector for ${enrolmentKey.keyType}: ${enrolmentKey.key}")
        res
    }
  }
}
