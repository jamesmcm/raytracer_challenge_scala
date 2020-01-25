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

import scala.collection.mutable
import scala.io.Source

class ObjParser {
  var defaultGroup: Group                    = Group()
  var vertices: List[RTTuple]                = List()
  var normals: List[RTTuple]                 = List()
  var curgroup: String                       = ""
  var groups: mutable.HashMap[String, Group] = new mutable.HashMap()
  val regex: scala.util.matching.Regex = "\\s+".r

  def parse(filename: String): Int = {
    var ignored_lines: Int = 0
    val bufferedSource     = Source.fromFile(filename)
    for (line <- bufferedSource.getLines) {
      line.headOption match {
        case None => { ignored_lines += 1 }
        case Some('v') => {
          line.lift(1) match {
            case Some(' ') => {
              val x: IndexedSeq[Double] =
                line.replaceAll("\\s+", " ").split(' ').drop(1).map((x: String) => x.toDouble).toIndexedSeq
              vertices = vertices :+ Point(x(0), x(1), x(2))
            }
            case Some('n') => {
              val x: IndexedSeq[Double] =
                line.replaceAll("\\s+", " ").split(' ').drop(1).map((x: String) => x.toDouble).toIndexedSeq
              normals = normals :+ Vector(x(0), x(1), x(2))
            }
            case _ => {
              ignored_lines += 1
            }
          }
        }
        case Some('f') => {
          line.split(' ').length match {
            case x if x == 4 => {
              if (line.contains('/')) {
                val x: IndexedSeq[(Int, Int)] =
                  line.replaceAll("\\s+", " ")
                    .split(' ')
                    .drop(1)
                    .map((x: String) => (x.split('/')(0).toInt - 1, x.split('/')(2).toInt - 1))
                    .toIndexedSeq
                if (curgroup.isEmpty) {
                  defaultGroup = defaultGroup.addChild(
                    SmoothTriangle(vertices(x(0)._1),
                                   vertices(x(1)._1),
                                   vertices(x(2)._1),
                                   normals(x(0)._2),
                                   normals(x(1)._2),
                                   normals(x(2)._2)))
                } else {
                  groups(curgroup) = groups(curgroup).addChild(
                    SmoothTriangle(vertices(x(0)._1),
                                   vertices(x(1)._1),
                                   vertices(x(2)._1),
                                   normals(x(0)._2),
                                   normals(x(1)._2),
                                   normals(x(2)._2)))
                }
              } else {
                val x: IndexedSeq[Int] =
                  line.replaceAll("\\s+", " ").split(' ').drop(1).map((x: String) => x.toInt - 1).toIndexedSeq
                if (curgroup.isEmpty) {
                  defaultGroup =
                    defaultGroup.addChild(Triangle(vertices(x(0)), vertices(x(1)), vertices(x(2))))
                } else {
                  groups(curgroup) = groups(curgroup).addChild(
                    Triangle(vertices(x(0)), vertices(x(1)), vertices(x(2))))
                }
              }
            }
            case x if x > 4 => {
              if (line.contains('/')) {
                val x: IndexedSeq[(Int, Int)] =
                  line.replaceAll("\\s+", " ")
                    .split(' ')
                    .drop(1)
                    .map((x: String) => (x.split('/')(0).toInt - 1, x.split('/')(2).toInt - 1))
                    .toIndexedSeq

                val startindex: Int  = x(0)._1
                val startnormal: Int = x(0)._2
                if (curgroup.isEmpty) {
                  defaultGroup = x
                    .drop(1)
                    .sliding(2)
                    .foldLeft(defaultGroup)(
                      (g: Group, x: Seq[(Int, Int)]) =>
                        g.addChild(
                          SmoothTriangle(vertices(startindex),
                                         vertices(x(0)._1),
                                         vertices(x(1)._1),
                                         normals(startnormal),
                                         normals(x(0)._2),
                                         normals(x(1)._1))))
                } else {
                  groups(curgroup) = x
                    .drop(1)
                    .sliding(2)
                    .foldLeft(defaultGroup)(
                      (g: Group, x: Seq[(Int, Int)]) =>
                        g.addChild(
                          SmoothTriangle(vertices(startindex),
                                         vertices(x(0)._1),
                                         vertices(x(1)._1),
                                         normals(startnormal),
                                         normals(x(0)._2),
                                         normals(x(1)._1))))
                }

              } else {
                val x: IndexedSeq[Int] =
                  line.replaceAll("\\s+", " ").split(' ').drop(1).map((x: String) => x.toInt - 1).toIndexedSeq
                val startindex: Int = x(0)
                if (curgroup.isEmpty) {
                  defaultGroup = x
                    .drop(1)
                    .sliding(2)
                    .foldLeft(defaultGroup)((g: Group, x: Seq[Int]) =>
                      g.addChild(Triangle(vertices(startindex), vertices(x(0)), vertices(x(1)))))
                } else {
                  groups(curgroup) = x
                    .drop(1)
                    .sliding(2)
                    .foldLeft(groups(curgroup))((g: Group, x: Seq[Int]) =>
                      g.addChild(Triangle(vertices(startindex), vertices(x(0)), vertices(x(1)))))
                }
              }
            }
            case _ => { ignored_lines += 1 }
          }
        }
        case Some('g') => {
          val name: String = line.replaceAll("\\s+", " ").split(' ')(1)
          curgroup = name
          groups(name) = Group()
        }
        case _ => { ignored_lines += 1 }
      }
    }

    bufferedSource.close()
    ignored_lines
  }

  def toGroup(): Group = {
    if (defaultGroup.objs.isEmpty) {
      groups.values.foldLeft(Group())((g: Group, x: Group) => g.addChild(x))
    } else {
      groups.values.foldLeft(defaultGroup)((g: Group, x: Group) => g.addChild(x))
    }
  }
}
