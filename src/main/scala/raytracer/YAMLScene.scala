// Copyright (C) 2019 James McMurray
//
// raytracer_challenge is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// raytracer_challenge is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with raytracer_challenge  If not, see <http://www.gnu.org/licenses/>.

package raytracer

import scala.io.Source
import io.circe.yaml.parser
import io.circe.parser
import io.circe._
import io.circe.Json
import net.liftweb.json.DefaultFormats
import net.liftweb.json._

final case class JSONPattern(
    typed: String,
    colors: List[List[Double]],
    transform: Option[List[List[String]]],
)

final case class JSONMaterial(
    colors: Option[List[Double]],
    ambient: Option[Double],
    diffuse: Option[Double],
    specular: Option[Double],
    shininess: Option[Double],
    reflective: Option[Double],
    transparency: Option[Double],
    refractive_index: Option[Double],
    pattern: Option[JSONPattern]
)

final case class JSONItem(
    add: String,
    transform: Option[List[List[String]]],
    material: Option[JSONMaterial],
    shadow: Option[Boolean]
)

object YAMLScene {
  def parseYAML(filename: String): Unit = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    val bufferedSource     = Source.fromFile(filename)
    val yamlString: String = bufferedSource.getLines.mkString("\n")
    bufferedSource.close()

    val myjson: Json       = io.circe.yaml.parser.parse(yamlString).getOrElse(Json.Null)
    val jsonString: String = myjson.spaces2

    val json     = parse(jsonString)
    val elements = (json).children
    for (acct <- elements) {
      val m = acct.extract[JSONItem]
      println(s"add: ${m.add}, transform: ${m.transform}, transform: ${m.material}")
      // Pattern match
      // Instantiate
      // Add to scene
      // Render
    }
  }

}
