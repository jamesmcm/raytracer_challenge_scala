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
  // drawClock(200)
  // castSphereSilhouette(100)
  lightSphere(800)


  def drawClock(radius: Double): Unit = {
    val canvas = Canvas(900, 500)
    val pointList: List[RTTuple] = List.fill(12)(Point(0, radius, 0)).zipWithIndex.map(
      (x: (RTTuple, Int)) => RotationZ(x._2 * (math.Pi / 6)).tupleMult(x._1))

    pointList.foreach((t: RTTuple) => canvas.writePixel((canvas.width/2) + math.round(t.x).toInt, (canvas.height/2) - math.round(t.y).toInt, Colour(1,0,0)))

    stringToFile("clock.ppm", canvas.toPPM)
  }

  def castSphereSilhouette(canvas_size: Int): Unit = {
    val canvas: Canvas = Canvas(canvas_size, canvas_size)
    val sphere: Sphere = Sphere.unitSphere().setTransform(Scaling(0.5,1,1))
    val ray_origin: RTTuple = Point(0, 0, -5)
    val wall_size: Double = 7.0
    val wall_z: Int = 10
    val pixel_size: Double = wall_size / canvas_size
    val half: Double = wall_size / 2

    val pixels: List[(Int, Int)] = (0 until canvas_size).flatMap((y: Int) => List(y).zipAll((0 until canvas_size), y, y)).toList

    pixels.foreach(
      (pix: (Int, Int)) => {
        val wall_target: RTTuple = Point(-half + pixel_size*pix._2, half - pixel_size * pix._1, wall_z)
        val ray: Ray = Ray(ray_origin, (wall_target - ray_origin).normalise())
        sphere.intersect(ray) match {
          case xs if xs.nonEmpty => canvas.writePixel(pix._2, pix._1, Colour(1,0,0)); Unit
          case _ => Unit
        }
      }
    ): Unit
    stringToFile("circle2.ppm", canvas.toPPM)
  }

  def lightSphere(canvas_size: Int): Unit = {
    val m: Material = Material.defaultMaterial().setColour(Colour(1, 0.2, 1))
    val light: Light = Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))

    val canvas: Canvas = Canvas(canvas_size, canvas_size)
    val sphere: Sphere = Sphere.unitSphere().setTransform(Scaling(0.5,1,1)).setMaterial(m)
    val ray_origin: RTTuple = Point(0, 0, -5)
    val wall_size: Double = 7.0
    val wall_z: Int = 10
    val pixel_size: Double = wall_size / canvas_size
    val half: Double = wall_size / 2

    val pixels: List[(Int, Int)] = (0 until canvas_size).flatMap((y: Int) => List(y).zipAll((0 until canvas_size), y, y)).toList

    pixels.foreach(
      (pix: (Int, Int)) => {
        val wall_target: RTTuple = Point(-half + pixel_size*pix._2, half - pixel_size * pix._1, wall_z)
        val ray: Ray = Ray(ray_origin, (wall_target - ray_origin).normalise())
        sphere.intersect(ray) match {
          case xs if xs.nonEmpty => canvas.writePixel(pix._2, pix._1,
            m.lighting(light,
            ray.position(Intersection.hit(xs).t),
            ray.direction.negate(),
            Intersection.hit(xs).shape.normalAt(ray.position(Intersection.hit(xs).t))
          )); Unit
          case _ => Unit
        }
      }
    ): Unit
    stringToFile("sphere.ppm", canvas.toPPM)
  }
}

