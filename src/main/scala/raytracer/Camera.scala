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

class Camera(val hsize: Int, val vsize: Int, val fov: Double, val transform: Matrix) {
  val half_view: Double         = math.tan(fov / 2)
  val aspect: Double            = hsize.toDouble / vsize.toDouble
  val transform_inverse: Matrix = transform.inverse

  val (half_width: Double, half_height: Double) = Camera.calculateHalves(half_view, aspect)

  val pixel_size: Double = (half_width * 2) / hsize.toDouble

  def setTransform(new_transform: Matrix): Camera = new Camera(hsize, vsize, fov, new_transform)

  def rayForPixel(px: Int, py: Int): Ray = {
    val xoffset: Double = (px.toDouble + 0.5) * pixel_size
    val yoffset: Double = (py.toDouble + 0.5) * pixel_size

    val world_x: Double = half_width - xoffset
    val world_y: Double = half_height - yoffset

    val pixel: RTTuple     = transform_inverse.tupleMult(Point(world_x, world_y, -1))
    val origin: RTTuple    = transform_inverse.tupleMult(Point(0, 0, 0))
    val direction: RTTuple = (pixel - origin).normalise()
    Ray(origin, direction)
  }

  def render(world: World): Canvas = {
    val c: Canvas = Canvas(hsize, vsize)

    val pixels: Array[(Int, Int)] =
      (0 until vsize).flatMap((y: Int) => List(y).zipAll((0 until hsize), y, y)).toArray

    pixels.par.foreach((px: (Int, Int)) => {
      c.writePixel(px._2, px._1, world.colourAt(rayForPixel(px._2, px._1), 5))
    })

    c
  }
}

object Camera {
  def apply(hsize: Int, vsize: Int, fov: Double): Camera =
    new Camera(hsize, vsize, fov, Matrix.getIdentityMatrix(4))

  def calculateHalves(half_view: Double, aspect: Double): (Double, Double) = {
    if (aspect >= 1) {
      (half_view, half_view / aspect)
    } else {
      (half_view * aspect, half_view)
    }
  }
}
