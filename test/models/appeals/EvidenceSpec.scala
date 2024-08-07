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

package models.appeals

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsValue, Json}

class EvidenceSpec extends AnyWordSpec with Matchers {
  val evidenceModelAsJson: JsValue = Json.parse(
    """
      |{
      |   "noOfUploadedFiles": 1
      |}
      |""".stripMargin)


  val evidenceModel: Evidence = Evidence(noOfUploadedFiles = 1)

  "be writable to JSON" in {
    val result = Json.toJson(evidenceModel)(Evidence.format)
    result shouldBe evidenceModelAsJson
  }

  "be readable from JSON" in {
    val result = Json.fromJson(evidenceModelAsJson)(Evidence.format)
    result.isSuccess shouldBe true
    result.get shouldBe evidenceModel
  }
}
