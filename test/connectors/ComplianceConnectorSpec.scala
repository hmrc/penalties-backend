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

package connectors

import base.{LogCapturing, SpecBase}
import config.AppConfig
import connectors.parsers.ComplianceParser.*
import models.EnrolmentKey
import models.TaxRegime.VAT
import models.compliance.{CompliancePayload, ComplianceStatusEnum, ObligationDetail, ObligationIdentification}
import org.mockito.Mockito.*
import org.mockito.ArgumentMatchers as Matchers
import play.api.test.Helpers.*
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{HeaderCarrier, UpstreamErrorResponse}
import utils.Logger.logger
import utils.PagerDutyHelper.PagerDutyKeys
import utils.TestUtils.given
import utils.VarargCaptor

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.{ExecutionContext, Future}

class ComplianceConnectorSpec extends SpecBase with LogCapturing {
  val testStartDate: LocalDateTime = LocalDateTime.of(2021,1,1,1,0,0)
  val testEndDate: LocalDateTime = LocalDateTime.of(2021,1,8,1,0,0)

  val date1: LocalDateTime = LocalDateTime.of(2022, 1, 1, 1, 1, 0)
  val date2: LocalDateTime = LocalDateTime.of(2024, 1, 1, 1, 1, 0)

  val vrn123456789: EnrolmentKey = EnrolmentKey(VAT, "123456789")

  class Setup {
    val mockHttpClient: HttpClientV2 = mock(classOf[HttpClientV2], RETURNS_DEEP_STUBS)
    val mockAppConfig: AppConfig = mock(classOf[AppConfig])

    val connector = new ComplianceConnector(mockHttpClient, mockAppConfig)(ExecutionContext.Implicits.global)

    val headersArgumentCaptor: VarargCaptor[(String, String)] = new VarargCaptor[(String, String)]

    when(mockAppConfig.getVatComplianceDataUrl(Matchers.eq("123456789"), Matchers.any(), Matchers.any())).thenReturn("http://foo/123456789")
    when(mockAppConfig.eisEnvironment).thenReturn("env")
    when(mockAppConfig.desBearerToken).thenReturn("12345")

    def mockGet[T](response: Future[T]): Unit = {
      when(mockHttpClient.get(
          Matchers.eq(s"http://foo/123456789")
        )(Matchers.any())
        .setHeader(headersArgumentCaptor.capture)
        .execute(Matchers.any(), Matchers.any())
      ).thenReturn(response)
    }
  }

  "getComplianceData" should {
    "should return a model - when the call succeeds and the body can be parsed" in new Setup {
      val compliancePayloadAsModel: CompliancePayload = CompliancePayload(
        identification = Some(ObligationIdentification(
          incomeSourceType = None,
          referenceNumber = "123456789",
          referenceType = "VRN"
        )),
        obligationDetails = Seq(
          ObligationDetail(
            status = ComplianceStatusEnum.open,
            inboundCorrespondenceFromDate = LocalDate.of(1920, 2, 29),
            inboundCorrespondenceToDate = LocalDate.of(1920, 2, 29),
            inboundCorrespondenceDateReceived = None,
            inboundCorrespondenceDueDate = LocalDate.of(1920, 2, 29),
            periodKey = "#001"
          ),
          ObligationDetail(
            status = ComplianceStatusEnum.fulfilled,
            inboundCorrespondenceFromDate = LocalDate.of(1920, 2, 29),
            inboundCorrespondenceToDate = LocalDate.of(1920, 2, 29),
            inboundCorrespondenceDateReceived = Some(LocalDate.of(1920, 2, 29)),
            inboundCorrespondenceDueDate = LocalDate.of(1920, 2, 29),
            periodKey = "#001"
          )
        )
      )
      mockGet(Future.successful(Right(CompliancePayloadSuccessResponse(compliancePayloadAsModel))))
      val result: CompliancePayloadResponse =
        await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
      result.isRight shouldBe true
      result.toOption.get.asInstanceOf[CompliancePayloadSuccessResponse] shouldBe CompliancePayloadSuccessResponse(compliancePayloadAsModel)
      headersArgumentCaptor.getValue.find(_._1 == "Authorization").get._2 shouldBe "Bearer 12345"
      headersArgumentCaptor.getValue.find(_._1 == "Environment").get._2 shouldBe "env"
    }

    "return a Left response" when {
      "the call returns a OK response however the body is not parsable as a model" in new Setup {
        mockGet(Future.successful(Left(CompliancePayloadMalformed)))
        val result: CompliancePayloadResponse =
          await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
        result.isLeft shouldBe true
      }

      "the call returns a Not Found status" in new Setup {
        mockGet(Future.successful(Left(CompliancePayloadNoData)))
        val result: CompliancePayloadResponse =
          await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
        result.isLeft shouldBe true
      }

      "the call returns a ISE" in new Setup {
        mockGet(Future.successful(Left(CompliancePayloadFailureResponse(INTERNAL_SERVER_ERROR))))
        val result: CompliancePayloadResponse =
          await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
        result.isLeft shouldBe true
      }

      "the call returns an unmatched response" in new Setup {
        mockGet(Future.successful(Left(CompliancePayloadFailureResponse(SERVICE_UNAVAILABLE))))
        val result: CompliancePayloadResponse =
          await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
        result.isLeft shouldBe true
      }

      "the call returns a UpstreamErrorResponse(4xx) exception" in new Setup {
        mockGet(Future.failed(UpstreamErrorResponse.apply("", BAD_REQUEST)))
        withCaptureOfLoggingFrom(logger) {
          logs => {
            val result: CompliancePayloadResponse =
              await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
            logs.exists(_.getMessage.contains(PagerDutyKeys.RECEIVED_4XX_FROM_1330_API.toString)) shouldBe true
            result.isLeft shouldBe true
          }
        }
      }

      "the call returns a UpstreamErrorResponse(5xx) exception" in new Setup {
        mockGet(Future.failed(UpstreamErrorResponse.apply("", INTERNAL_SERVER_ERROR)))
        withCaptureOfLoggingFrom(logger) {
          logs => {
            val result: CompliancePayloadResponse =
              await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
            logs.exists(_.getMessage.contains(PagerDutyKeys.RECEIVED_5XX_FROM_1330_API.toString)) shouldBe true
            result.isLeft shouldBe true
          }
        }
      }

      "the call returns an exception" in new Setup {
        mockGet(Future.failed(new Exception("failed")))
        withCaptureOfLoggingFrom(logger) {
          logs => {
            val result: CompliancePayloadResponse =
              await(connector.getComplianceData(vrn123456789, "2020-01-01", "2020-12-31")(HeaderCarrier()))
            logs.exists(_.getMessage.contains(PagerDutyKeys.UNKNOWN_EXCEPTION_CALLING_1330_API.toString)) shouldBe true
            result.isLeft shouldBe true
          }
        }
      }
    }
  }
}
