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

import scala.annotation.tailrec

class ParticleEnvironment(val gravity: RTTuple, wind: RTTuple) {
  def tick(p: Projectile): Projectile = {
    Projectile(p.position + p.velocity, p.velocity + gravity + wind)
  }

  @tailrec
  final def ticksToLand(p: Projectile, acc: Int): Int = {
    p.position.y match {
      case y if y < 0 || doubleEq(y, 0) => acc
      case _                            => ticksToLand(tick(p), acc + 1)
    }
  }

  @tailrec
  final def ticksInBoundsToCanvas(canvas: Canvas, p: Projectile, col: Colour): Unit = {
    p.position.toTuple match {
      case (x, y, z, w) if Math.round(x).toInt >= canvas.width || Math.round(x).toInt < 0  => ()
      case (x, y, z, w) if Math.round(y).toInt >= canvas.height || Math.round(y).toInt < 0 => ()
      case (x, y, z, w) => {
        canvas.writePixel(Math.round(x).toInt, canvas.height - Math.round(y).toInt, col);
        ticksInBoundsToCanvas(canvas, tick(p), col);
      }
    }
  }

}

object ParticleEnvironment {

  def apply(gravity: RTTuple, wind: RTTuple): ParticleEnvironment =
    new ParticleEnvironment(gravity, wind)

  def drawParticleTest(): Unit = {
    val start    = Point(0, 1, 0)
    val velocity = Vector(1, 1.8, 0).normalise() * 11.25
    val p        = Projectile(start, velocity)

    val e      = ParticleEnvironment(Vector(0, -0.1, 0), Vector(-0.01, 0, 0))
    val canvas = Canvas(900, 500)

    e.ticksInBoundsToCanvas(canvas, p, Colour(1, 0, 0))
    stringToFile("test1.ppm", canvas.toPPM)
  }
}

final case class Projectile(position: RTTuple, velocity: RTTuple)
