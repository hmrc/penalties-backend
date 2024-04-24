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

package models

import models.EnrolmentKey.KeyType
import models.TaxRegime.{CT, ITSA, TaxRegime, VAT}
import play.api.mvc.QueryStringBindable.{Parsing => QueryStringParsing}
import play.api.mvc.PathBindable.{Parsing => PathParsing}
import utils.Logger.logger

import scala.util.matching.Regex

object EnrolmentKey {
  sealed case class KeyType(name: String, regEx: Regex) {
    override def toString: String = name
  }

  object VRN extends KeyType("VRN", "^[0-9]{9}|[0-9]{12}$".r)
  object UTR extends KeyType("UTR", "^[0-9A-Z]{10}$".r)

  private def defaultKeyType(regime: TaxRegime) = regime match {
    case VAT => VRN
    case ITSA | CT => UTR
    case _ => throw new Exception(s"Unexpected regime: $regime")
  }

  private val enrolmentKeyRegex: Regex = """HMRC-MTD-(?<regime>[A-Z]+)~(?<type>[A-Z]+)~(?<key>[0-9A-Z]+)""".r

  /** parse an enrolment key supplied as a string */
  def apply(key: String): EnrolmentKey = key match {
    case enrolmentKeyRegex(regime, "VRN", key) => new EnrolmentKey(TaxRegime.withName(regime), VRN, key)
    case enrolmentKeyRegex(regime, "UTR", key) => new EnrolmentKey(TaxRegime.withName(regime), UTR, key)
    case _ => throw new Exception(s"Invalid enrolment key: $key")
  }

  /** construct an enrolment key, or None if invalid */
  def apply(regime: TaxRegime, keyType: KeyType, key: String): Option[EnrolmentKey] =
    try {
      Some(new EnrolmentKey(regime, keyType, key))
    } catch {
      case e: Exception =>
        logger.debug(e.getMessage)
        None
    }

  /** construct an enrolment key, assuming the default key type for the regime, or None if invalid */
  def apply(regime: TaxRegime, key: String): Option[EnrolmentKey] =
    EnrolmentKey(regime, defaultKeyType(regime), key)

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
    case (ITSA, UTR) if UTR.regEx.matches(key) => //ok
    case _ => throw new Exception(s"Invalid key for $regime, $keyType: $key")
  }

  override def toString: String = s"HMRC-MTD-$regime~$keyType~$key"

  /** this seems to be the format preferred in logging */
  def info: String = s"$keyType: $key"
}
