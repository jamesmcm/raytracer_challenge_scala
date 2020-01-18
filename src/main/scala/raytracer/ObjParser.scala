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


class ObjParser {
  var defaultGroup: Group = Group()
  var vertices: List[RTTuple] = List()

  def parse(filename: String): Int = {
  var ignored_lines: Int = 0
  val bufferedSource = Source.fromFile(filename)

  for (line <- bufferedSource.getLines) {
    line.headOption match {
      case None => {ignored_lines += 1}
      case Some('v') => {
        val x: IndexedSeq[Double] = line.split(' ').drop(1).map((x: String) => x.toDouble).toIndexedSeq
        vertices = vertices :+ Point(x(0),x(1),x(2))
      }
      case Some('f') => {
        line.split(' ').length match {
          case x if x == 4 => {
            val x: IndexedSeq[Int] = line.split(' ').drop(1).map((x: String) => x.toInt - 1).toIndexedSeq
            // TODO: Make this handle group selection
            defaultGroup = defaultGroup.addChild(Triangle(vertices(x(0)), vertices(x(1)), vertices(x(2))))
          }
          case _ => {ignored_lines += 1}
        }
      }
      case _ => {ignored_lines += 1}
    }
    }

    bufferedSource.close()
    ignored_lines
  }
}
