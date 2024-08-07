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

package utils

import config.AppConfig
import org.apache.commons.io.FilenameUtils

object FileHelper {
  def appendFileExtension(fileName: String)(fileMimeType: String)(fileReference: String)(implicit appConfig: AppConfig): String = {
    val fileExtension = FilenameUtils.getExtension(fileName)
    if (fileExtension.isEmpty) {
      val mimeTypeWithoutForwardSlash = fileMimeType.replace("/", ".")
      appConfig.getMimeType(mimeTypeWithoutForwardSlash) match {
        case Some(fileExtensionFromMimeType) => fileName ++ fileExtensionFromMimeType
        case None => throw new Exception(s"[FileHelper][appendFileExtension] - Unknown mime type: $fileMimeType for file reference: $fileReference")
      }
    } else {
      fileName
    }
  }
}
