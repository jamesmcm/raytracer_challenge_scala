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

object Main extends App {
  // ParticleEnvironment.drawParticleTest()
  drawClock(200)

  def drawClock(radius: Double): Unit = {
    val canvas = Canvas(900, 500)
    val pointList: List[RTTuple] = List.fill(12)(Point(0, radius, 0)).zipWithIndex.map(
      (x: (RTTuple, Int)) => RotationZ(x._2 * (math.Pi / 6)).tupleMult(x._1))

    pointList.foreach((t: RTTuple) => canvas.writePixel((canvas.width/2) + math.round(t.x).toInt, (canvas.height/2) - math.round(t.y).toInt, Colour(1,0,0)))

    stringToFile("clock.ppm", canvas.toPPM)
  }
}

