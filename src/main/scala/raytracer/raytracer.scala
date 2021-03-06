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

package object raytracer {
  val EPSILON: Double = 0.00001;

  def doubleEq(x: Double, y: Double): Boolean = {
    if (Math.abs(x - y) <= EPSILON) true else false
  }

  def stringToFile(filename: String, output: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(filename))
    pw.write(output)
    pw.close()
  }
}
