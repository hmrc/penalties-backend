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

package models

import models.EnrolmentKey.KeyType
import models.TaxRegime._
import play.api.mvc.PathBindable.{Parsing => PathParsing}
import play.api.mvc.QueryStringBindable.{Parsing => QueryStringParsing}

import scala.util.matching.Regex

/**
 * @see "https://github.com/hmrc/manage-services-frontend/blob/main/conf/messages"
 */
object EnrolmentKey {
  sealed case class KeyType(name: String, regEx: Regex) {
    override def toString: String = name
  }

  object VRN extends KeyType("VRN", "^[0-9]{9}|[0-9]{12}$".r)
  //object UTR extends KeyType("UTR", "^[0-9A-Z]{10}$".r)
  object NINO extends KeyType("NINO", "^[ABCEGHJKLMNOPRSTWXYZ][ABCEGHJKLMNPRSTWXYZ][0-9]{6}[A-D\\s]$".r)

  private def defaultKeyType(regime: TaxRegime) = regime match {
    case VAT => VRN
    //case CT => UTR
    case ITSA => NINO
    case _ => throw new Exception(s"Unexpected regime: $regime")
  }

  private val enrolmentKeyRegex: Regex = """(?<regime>[A-Z\\-]+)~(?<type>[A-Z]+)~(?<key>[0-9A-Z]+)""".r

  /** parse an enrolment key supplied as a string */
  def apply(key: String): EnrolmentKey = key match {
    case enrolmentKeyRegex("HMRC-MTD-VAT", "VRN", key) => new EnrolmentKey(VAT, VRN, key)
    //case enrolmentKeyRegex("IR-CT", "UTR", key) => new EnrolmentKey(CT, UTR, key)
    case enrolmentKeyRegex("HMRC-PT", "NINO", key) => new EnrolmentKey(ITSA, NINO, key)
    case _ => throw new Exception(s"Invalid enrolment key: $key")
  }

  /** construct an enrolment key */
  def apply(regime: TaxRegime, keyType: KeyType, key: String): EnrolmentKey = new EnrolmentKey(regime, keyType, key)

  /** construct an enrolment key, assuming the default key type for the regime */
  def apply(regime: TaxRegime, key: String): EnrolmentKey = EnrolmentKey(regime, defaultKeyType(regime), key)

  def unapply(enrolmentKey: EnrolmentKey): Option[(TaxRegime, KeyType, String)] = Some((enrolmentKey.regime, enrolmentKey.keyType, enrolmentKey.key))

  /** Supports using Enrolment Keys in query string parameters */
  implicit def queryStringBinder: QueryStringParsing[EnrolmentKey] =
    new QueryStringParsing({ str => EnrolmentKey(str) }, { ek: EnrolmentKey => ek.toString }, { case (msg, _) => msg })

  /** Supports using Enrolment Keys in route paths */
  implicit def pathBinder: PathParsing[EnrolmentKey] =
    new PathParsing({ str => EnrolmentKey(str) }, { ek: EnrolmentKey => ek.toString }, { case (msg, _) => msg })
}

case class EnrolmentKey(regime: TaxRegime, keyType: KeyType, key: String) {
  import EnrolmentKey._

  // validate on construction
  (regime, keyType)  match {
    case (VAT, VRN) if VRN.regEx.matches(key) => //ok
    //case (CT, UTR) if UTR.regEx.matches(key) => //ok
    case (ITSA, NINO) if NINO.regEx.matches(key) => //ok
    case _ => throw new Exception(s"Invalid $regime $keyType: $key")
  }

  override def toString: String = regime match {
    case VAT  => s"HMRC-MTD-VAT~$keyType~$key"
    //case CT   => s"IR-CT~$keyType~$key"
    case ITSA => s"HMRC-PT~$keyType~$key"
    case _ => throw new Exception(s"Unsupported regime: $regime")
  }

  /** this seems to be the format preferred in logging */
  def info: String = s"$keyType: $key"
}
