/*
 * Copyright 2023 HM Revenue & Customs
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

package models.appeals

import models.upload.UploadJourney
import play.api.libs.json._
import utils.DateHelper.addUtcTimeZone

import java.time.{LocalDateTime, ZoneOffset}

sealed trait AppealInformation {
  val statement: Option[String]
  val reasonableExcuse: String
  val honestyDeclaration: Boolean
  val isClientResponsibleForSubmission: Option[Boolean]
  val isClientResponsibleForLateSubmission: Option[Boolean]
}

case class BereavementAppealInformation(
                                         startDateOfEvent: String,
                                         statement: Option[String],
                                         lateAppeal: Boolean,
                                         lateAppealReason: Option[String],
                                         reasonableExcuse: String,
                                         honestyDeclaration: Boolean,
                                         isClientResponsibleForSubmission: Option[Boolean],
                                         isClientResponsibleForLateSubmission: Option[Boolean]
                                       ) extends AppealInformation

object BereavementAppealInformation {
  implicit val bereavementAppealInformationFormatter: OFormat[BereavementAppealInformation] = Json.format[BereavementAppealInformation]

  val bereavementAppealWrites: Writes[BereavementAppealInformation] = bereavementAppealInformationFormatter.contramap(orig => orig.copy(
    startDateOfEvent = addUtcTimeZone(orig.startDateOfEvent)
  ))
}

case class CrimeAppealInformation(
                                   startDateOfEvent: String,
                                   reportedIssueToPolice: String,
                                   statement: Option[String],
                                   lateAppeal: Boolean,
                                   lateAppealReason: Option[String],
                                   reasonableExcuse: String,
                                   honestyDeclaration: Boolean,
                                   isClientResponsibleForSubmission: Option[Boolean],
                                   isClientResponsibleForLateSubmission: Option[Boolean]
                                 ) extends AppealInformation

object CrimeAppealInformation {
  implicit val crimeAppealInformationFormatter: OFormat[CrimeAppealInformation] = Json.format[CrimeAppealInformation]

  val crimeAppealWrites: Writes[CrimeAppealInformation] = crimeAppealInformationFormatter.contramap(orig => orig.copy(
    startDateOfEvent = addUtcTimeZone(orig.startDateOfEvent)
  ))
}

case class FireOrFloodAppealInformation(
                                         startDateOfEvent: String,
                                         statement: Option[String],
                                         lateAppeal: Boolean,
                                         lateAppealReason: Option[String],
                                         reasonableExcuse: String,
                                         honestyDeclaration: Boolean,
                                         isClientResponsibleForSubmission: Option[Boolean],
                                         isClientResponsibleForLateSubmission: Option[Boolean]
                                       ) extends AppealInformation

object FireOrFloodAppealInformation {
  implicit val fireOrFloodAppealInformationFormatter: OFormat[FireOrFloodAppealInformation] = Json.format[FireOrFloodAppealInformation]

  val fireOrFloodAppealWrites: Writes[FireOrFloodAppealInformation] = fireOrFloodAppealInformationFormatter.contramap(orig => orig.copy(
    startDateOfEvent = addUtcTimeZone(orig.startDateOfEvent)
  ))
}

case class LossOfStaffAppealInformation(
                                         startDateOfEvent: String,
                                         statement: Option[String],
                                         lateAppeal: Boolean,
                                         lateAppealReason: Option[String],
                                         reasonableExcuse: String,
                                         honestyDeclaration: Boolean,
                                         isClientResponsibleForSubmission: Option[Boolean],
                                         isClientResponsibleForLateSubmission: Option[Boolean]
                                       ) extends AppealInformation

object LossOfStaffAppealInformation {
  implicit val lossOfStaffAppealInformationFormatter: OFormat[LossOfStaffAppealInformation] = Json.format[LossOfStaffAppealInformation]

  val lossOfStaffAppealWrites: Writes[LossOfStaffAppealInformation] = lossOfStaffAppealInformationFormatter.contramap(orig => orig.copy(
    startDateOfEvent = addUtcTimeZone(orig.startDateOfEvent)
  ))
}

case class TechnicalIssuesAppealInformation(
                                             startDateOfEvent: String,
                                             endDateOfEvent: String,
                                             statement: Option[String],
                                             lateAppeal: Boolean,
                                             lateAppealReason: Option[String],
                                             reasonableExcuse: String,
                                             honestyDeclaration: Boolean,
                                             isClientResponsibleForSubmission: Option[Boolean],
                                             isClientResponsibleForLateSubmission: Option[Boolean]
                                           ) extends AppealInformation

object TechnicalIssuesAppealInformation {
  implicit val technicalIssuesAppealInformationFormatter: OFormat[TechnicalIssuesAppealInformation] = Json.format[TechnicalIssuesAppealInformation]

  val technicalIssuesAppealWrites: Writes[TechnicalIssuesAppealInformation] = technicalIssuesAppealInformationFormatter.contramap(orig => orig.copy(
    startDateOfEvent = addUtcTimeZone(orig.startDateOfEvent), endDateOfEvent = addUtcTimeZone(orig.endDateOfEvent)
  ))
}

case class HealthAppealInformation(
                                    startDateOfEvent: Option[String],
                                    endDateOfEvent: Option[String],
                                    eventOngoing: Option[Boolean],
                                    statement: Option[String],
                                    lateAppeal: Boolean,
                                    hospitalStayInvolved: Boolean,
                                    lateAppealReason: Option[String],
                                    reasonableExcuse: String,
                                    honestyDeclaration: Boolean,
                                    isClientResponsibleForSubmission: Option[Boolean],
                                    isClientResponsibleForLateSubmission: Option[Boolean]
                                  ) extends AppealInformation

object HealthAppealInformation {
  implicit val healthAppealInformationFormatter: OFormat[HealthAppealInformation] = Json.format[HealthAppealInformation]

  val healthAppealWrites: Writes[HealthAppealInformation] = healthAppealInformationFormatter.transform{jso: JsObject =>
    jso.-("hospitalStayInvolved")
  }.contramap(orig => orig.copy(
    reasonableExcuse = if (orig.hospitalStayInvolved) "unexpectedHospitalStay" else "seriousOrLifeThreateningIllHealth",
    startDateOfEvent = orig.startDateOfEvent.map(addUtcTimeZone),
    endDateOfEvent = orig.endDateOfEvent.map(addUtcTimeZone),
    eventOngoing = if (orig.hospitalStayInvolved) orig.eventOngoing else None,
  ))
}

case class OtherAppealInformation(
                                   startDateOfEvent: String,
                                   statement: Option[String],
                                   supportingEvidence: Option[Evidence],
                                   lateAppeal: Boolean,
                                   lateAppealReason: Option[String],
                                   reasonableExcuse: String,
                                   honestyDeclaration: Boolean,
                                   isClientResponsibleForSubmission: Option[Boolean],
                                   isClientResponsibleForLateSubmission: Option[Boolean],
                                   uploadedFiles: Option[Seq[UploadJourney]]
                                 ) extends AppealInformation

object OtherAppealInformation {
  implicit val evidenceFormatter: OFormat[Evidence] = Evidence.format
  implicit val otherAppealInformationFormatter: OFormat[OtherAppealInformation] = Json.format[OtherAppealInformation]

  val otherAppealInformationWrites: Writes[OtherAppealInformation] = otherAppealInformationFormatter.contramap(orig => orig.copy(
    startDateOfEvent = addUtcTimeZone(orig.startDateOfEvent)
  ))
}

case class AppealSubmission(
                             taxRegime: String,
                             appealSubmittedBy: String,
                             customerReferenceNo: String,
                             dateOfAppeal: LocalDateTime,
                             agentDetails: Option[AgentDetails],
                             isLPP: Boolean,
                             appealInformation: AppealInformation
                           ) {
  val sourceSystem: String = "MDTP"
}

object AppealSubmission {
  def parseAppealInformationFromJson(reason: String, payload: JsValue): JsResult[AppealInformation] = {
    reason match {
      case "bereavement" =>
        Json.fromJson(payload)(BereavementAppealInformation.bereavementAppealInformationFormatter)
      case "crime" =>
        Json.fromJson(payload)(CrimeAppealInformation.crimeAppealInformationFormatter)
      case "fireandflood" =>
        Json.fromJson(payload)(FireOrFloodAppealInformation.fireOrFloodAppealInformationFormatter)
      case "lossOfEssentialStaff" =>
        Json.fromJson(payload)(LossOfStaffAppealInformation.lossOfStaffAppealInformationFormatter)
      case "technicalIssue" =>
        Json.fromJson(payload)(TechnicalIssuesAppealInformation.technicalIssuesAppealInformationFormatter)
      case "health" =>
        Json.fromJson(payload)(HealthAppealInformation.healthAppealInformationFormatter)
      case "other" =>
        Json.fromJson(payload)(OtherAppealInformation.otherAppealInformationFormatter)
    }
  }

  def parseAppealInformationToJson(payload: AppealInformation): JsValue = {
    payload.reasonableExcuse match {
      case "bereavement" =>
        Json.toJson(payload.asInstanceOf[BereavementAppealInformation])(BereavementAppealInformation.bereavementAppealWrites)
      case "crime" =>
        Json.toJson(payload.asInstanceOf[CrimeAppealInformation])(CrimeAppealInformation.crimeAppealWrites)
      case "fireandflood" =>
        Json.toJson(payload.asInstanceOf[FireOrFloodAppealInformation])(FireOrFloodAppealInformation.fireOrFloodAppealWrites)
      case "lossOfEssentialStaff" =>
        Json.toJson(payload.asInstanceOf[LossOfStaffAppealInformation])(LossOfStaffAppealInformation.lossOfStaffAppealWrites)
      case "technicalIssue" =>
        Json.toJson(payload.asInstanceOf[TechnicalIssuesAppealInformation])(TechnicalIssuesAppealInformation.technicalIssuesAppealWrites)
      case "health" =>
        Json.toJson(payload.asInstanceOf[HealthAppealInformation])(HealthAppealInformation.healthAppealWrites)
      case "other" =>
        Json.toJson(payload.asInstanceOf[OtherAppealInformation])(OtherAppealInformation.otherAppealInformationWrites)
    }
  }

  val apiReads: Reads[AppealSubmission] = (json: JsValue) => {
    for {
      taxRegime <- (json \ "taxRegime").validate[String]
      appealSubmittedBy <- (json \ "appealSubmittedBy").validate[String]
      customerReferenceNo <- (json \ "customerReferenceNo").validate[String]
      dateOfAppeal <- (json \ "dateOfAppeal").validate[LocalDateTime]
      isLPP <- (json \ "isLPP").validate[Boolean]
      appealInformationType <- (json \ "appealInformation" \ "reasonableExcuse").validate[String]
      agentDetails <- (json \ "agentDetails").validateOpt[AgentDetails]
      appealInformation <- parseAppealInformationFromJson(appealInformationType, (json \ "appealInformation").get)
    } yield {
      AppealSubmission(
        taxRegime,
        appealSubmittedBy,
        customerReferenceNo,
        dateOfAppeal,
        agentDetails,
        isLPP,
        appealInformation
      )
    }
  }

  implicit val apiWrites: Writes[AppealSubmission] = (appealSubmission: AppealSubmission) => {
    val dateOfAppealZoned: String = appealSubmission.dateOfAppeal.toInstant(ZoneOffset.UTC).toString
    Json.obj(
      "sourceSystem" -> appealSubmission.sourceSystem,
      "taxRegime" -> appealSubmission.taxRegime,
      "customerReferenceNo" -> appealSubmission.customerReferenceNo,
      "dateOfAppeal" -> dateOfAppealZoned,
      "isLPP" -> appealSubmission.isLPP,
      "appealSubmittedBy" -> appealSubmission.appealSubmittedBy,
      "appealInformation" -> parseAppealInformationToJson(appealSubmission.appealInformation)
    ).deepMerge(
      appealSubmission.agentDetails.fold(Json.obj())(agentDetails => Json.obj("agentDetails" -> agentDetails))
    )
  }
}
