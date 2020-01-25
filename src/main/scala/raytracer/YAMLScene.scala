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
import cats.implicits._

final case class JSONPattern(
    typed: String,
    colors: List[List[Double]],
    transform: Option[List[List[String]]],
)

final case class JSONMaterial(
    color: Option[List[Double]],
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
    at: Option[List[Double]],
    intensity: Option[List[Double]],
    material: Option[JSONMaterial],
    width: Option[Int],
    height: Option[Int],
    field_of_view: Option[Double],
    from: Option[List[Double]],
    to: Option[List[Double]],
    up: Option[List[Double]],
    shadow: Option[Boolean],
    min: Option[Double],
    max: Option[Double],
    closed: Option[Boolean],
    bounds: Option[Boolean],
    p1: Option[List[Double]],
    p2: Option[List[Double]],
    p3: Option[List[Double]],
    filename: Option[String],
    // objs: Option[List[JSONItem]], TODO: groups
)

object YAMLScene {

  def listToTuple[T](l: Option[List[T]]): (T, T, T) = {
    (l.getOrElse(List())(0), l.getOrElse(List())(1), l.getOrElse(List())(2))
  }

  def getMaterial(mat: Option[JSONMaterial]): Material = {
    mat match {
      case None => Material.defaultMaterial()
      case Some(m) => {

        var outm: Material = Material.defaultMaterial()
        m.ambient match {
          case None            => {}
          case Some(x: Double) => outm = outm.setAmbient(x)
        }
        m.diffuse match {
          case None            => {}
          case Some(x: Double) => outm = outm.setDiffuse(x)
        }
        m.specular match {
          case None            => {}
          case Some(x: Double) => outm = outm.setSpecular(x)
        }
        m.shininess match {
          case None            => {}
          case Some(x: Double) => outm = outm.setShininess(x)
        }
        m.reflective match {
          case None            => {}
          case Some(x: Double) => outm = outm.setReflective(x)
        }
        m.transparency match {
          case None            => {}
          case Some(x: Double) => outm = outm.setTransparency(x)
        }
        m.refractive_index match {
          case None            => {}
          case Some(x: Double) => outm = outm.setRefractiveIndex(x)
        }
        m.color match {
          case None                  => {}
          case Some(x: List[Double]) => outm = outm.setColour(Colour(x(0), x(1), x(2)))
        }
        m.pattern match {
          case None                 => {}
          case Some(x: JSONPattern) => outm = outm.setPattern(getPattern(x))
        }
        outm
      }
    }
  }

  def getTransform(t: Option[List[List[String]]]): Matrix = {
    t match {
      case None => Matrix.getIdentityMatrix(4)
      case Some(x) =>
        x.map((z: List[String]) =>
            z(0) match {
              case "scale"     => Scaling(z(1).toDouble, z(2).toDouble, z(3).toDouble)
              case "translate" => Translation(z(1).toDouble, z(2).toDouble, z(3).toDouble)
              case "rotate-y"  => RotationY(z(1).toDouble)
              case "rotate-x"  => RotationX(z(1).toDouble)
              case "rotate-z"  => RotationZ(z(1).toDouble)
          })
          .reverse
          .foldLeft(Matrix.getIdentityMatrix(4))((x: Matrix, y: Matrix) => x * y)
    }
  }

  def getPattern(p: JSONPattern): Pattern = {
    val pattern: Pattern = p.typed match {
      case "stripes" =>
        StripePattern(Colour(p.colors(0)(0), p.colors(0)(1), p.colors(0)(2)),
                      Colour(p.colors(1)(0), p.colors(1)(1), p.colors(1)(2)))
      case "checkers" =>
        CheckeredPattern(Colour(p.colors(0)(0), p.colors(0)(1), p.colors(0)(2)),
                         Colour(p.colors(1)(0), p.colors(1)(1), p.colors(1)(2)))
    }
    pattern.setTransform(getTransform(p.transform))
  }

  def parseYAMLToScene(filename: String): (Camera, World) = {
    implicit val formats: DefaultFormats.type = DefaultFormats

    val bufferedSource     = Source.fromFile(filename)
    val yamlString: String = bufferedSource.getLines.mkString("\n")
    bufferedSource.close()

    val myjson: Json       = io.circe.yaml.parser.parse(yamlString).getOrElse(Json.Null)
    val jsonString: String = myjson.spaces2

    val json                    = parse(jsonString)
    val elements                = (json).children
    var lights: List[Light]     = List()
    var camera: Camera          = Camera(0, 0, 0)
    var objs: List[SpaceObject] = List()
    for (acct <- elements) {
      val m = acct.extract[JSONItem]
      // DEBUG: println(s"add: ${m.add}: ${m}")
      // Pattern match
      m match {
        case m if m.add === "camera" => {
          val from = listToTuple(m.from); val to = listToTuple(m.to); val up = listToTuple(m.up);
          camera = Camera(m.width.getOrElse(0), m.height.getOrElse(0), m.field_of_view.getOrElse(0));
          camera = camera.setTransform(
            viewTransform(Point(from._1, from._2, from._3),
                          Point(to._1, to._2, to._3),
                          Vector(up._1, up._2, up._3)))
        }
        case m if m.add === "light" => {
          val t = listToTuple(m.at); val i = listToTuple(m.intensity);
          lights = lights :+ Light.pointLight(Point(t._1, t._2, t._3),
                                              intensity = Colour(i._1, i._2, i._3))
        }
        case m if m.add === "cube" => {
          objs = objs :+ Cube()
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
        }
        case m if m.add === "plane" => {
          objs = objs :+ Plane()
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
        }
        case m if m.add === "cylinder" => {
          objs = objs :+ Cylinder()
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
            .setClosed(m.closed match { case None => true; case Some(x: Boolean) => x; })
            .setMaximum(m.max match {
              case None => Double.PositiveInfinity; case Some(x: Double) => x;
            })
            .setMinimum(m.min match {
              case None => Double.NegativeInfinity; case Some(x: Double) => x;
            })
        }

        case m if m.add === "cone" => {
          objs = objs :+ Cone()
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
            .setClosed(m.closed match { case None => true; case Some(x: Boolean) => x; })
            .setMaximum(m.max match {
              case None => Double.PositiveInfinity; case Some(x: Double) => x;
            })
            .setMinimum(m.min match {
              case None => Double.NegativeInfinity; case Some(x: Double) => x;
            })
        }

        case m if m.add === "hexagon_ring" => {
          val hex: Group = HexagonRing
            .hexagon(getMaterial(m.material))
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })

          val useBounds: Boolean = m.bounds match {
            case None => false; case Some(x: Boolean) => x;
          }

          objs = objs :+ (if (useBounds) hex.setBounds() else hex)
        }

        case m if m.add === "sphere" => {
          objs = objs :+ Sphere
            .unitSphere()
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
        }
        case m if m.add === "triangle" => {
          val p1: List[Double] = m.p1.get
          val p2: List[Double] = m.p2.get
          val p3: List[Double] = m.p3.get

          objs = objs :+ Triangle(
            Point(p1(0), p1(1), p1(2)),
            Point(p2(0), p2(1), p2(2)),
            Point(p3(0), p3(1), p3(2))
          ).setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
        }
        case m if m.add === "object" => {
          val parser: ObjParser  = new ObjParser
          val ignored_lines: Int = parser.parse(m.filename.get)
          val g: Group           = parser.toGroup().setBounds()
          objs = objs :+ g
            .setTransform(getTransform(m.transform))
            .setMaterial(getMaterial(m.material))
            .setShadow(m.shadow match { case None => true; case Some(x: Boolean) => x; })
        }
      }
      // Instantiate
    }
    val world: World = World(lights, objs)
    (camera, world)
  }

}
