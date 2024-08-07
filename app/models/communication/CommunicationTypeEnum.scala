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

package models.communication

import play.api.libs.json._

object CommunicationTypeEnum extends Enumeration {
  val secureMessage: CommunicationTypeEnum.Value = Value
  val letter: CommunicationTypeEnum.Value = Value

  implicit val format: Format[CommunicationTypeEnum.Value] = new Format[CommunicationTypeEnum.Value] {
    override def writes(o: CommunicationTypeEnum.Value): JsValue = {
      JsString(o.toString)
    }

    override def reads(json: JsValue): JsResult[CommunicationTypeEnum.Value] = {
      json.as[String] match {
        case "secureMessage" => JsSuccess(secureMessage)
        case "letter" => JsSuccess(letter)
        case e => JsError(s"$e not recognised")
      }
    }
  }
}
