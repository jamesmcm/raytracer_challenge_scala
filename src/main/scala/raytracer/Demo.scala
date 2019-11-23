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

object Demo {
  def drawClock(radius: Double): Unit = {
    val canvas = Canvas(900, 500)
    val pointList: List[RTTuple] = List
      .fill(12)(Point(0, radius, 0))
      .zipWithIndex
      .map((x: (RTTuple, Int)) => RotationZ(x._2 * (math.Pi / 6)).tupleMult(x._1))

    pointList.foreach(
      (t: RTTuple) =>
        canvas.writePixel((canvas.width / 2) + math.round(t.x).toInt,
                          (canvas.height / 2) - math.round(t.y).toInt,
                          Colour(1, 0, 0)))

    stringToFile("clock.ppm", canvas.toPPM)
  }

  def castSphereSilhouette(canvas_size: Int): Unit = {
    val canvas: Canvas      = Canvas(canvas_size, canvas_size)
    val sphere: Sphere      = Sphere.unitSphere().setTransform(Scaling(0.5, 1, 1))
    val ray_origin: RTTuple = Point(0, 0, -5)
    val wall_size: Double   = 7.0
    val wall_z: Int         = 10
    val pixel_size: Double  = wall_size / canvas_size
    val half: Double        = wall_size / 2

    val pixels: List[(Int, Int)] =
      (0 until canvas_size).flatMap((y: Int) => List(y).zipAll((0 until canvas_size), y, y)).toList

    pixels.foreach(
      (pix: (Int, Int)) => {
        val wall_target: RTTuple =
          Point(-half + pixel_size * pix._2, half - pixel_size * pix._1, wall_z)
        val ray: Ray = Ray(ray_origin, (wall_target - ray_origin).normalise())
        sphere.intersect(ray) match {
          case xs if xs.nonEmpty => canvas.writePixel(pix._2, pix._1, Colour(1, 0, 0)); Unit
          case _                 => Unit
        }
      }
    ): Unit
    stringToFile("circle2.ppm", canvas.toPPM)
  }

  def lightSphere(canvas_size: Int): Unit = {
    val m: Material  = Material.defaultMaterial().setColour(Colour(0.2, 0.2, 1))
    val light: Light = Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))

    val canvas: Canvas      = Canvas(canvas_size, canvas_size)
    val sphere: Sphere      = Sphere.unitSphere().setMaterial(m)
    val ray_origin: RTTuple = Point(0, 0, -5)
    val wall_size: Double   = 7.0
    val wall_z: Int         = 10
    val pixel_size: Double  = wall_size / canvas_size
    val half: Double        = wall_size / 2

    val pixels: Array[(Int, Int)] =
      (0 until canvas_size).flatMap((y: Int) => List(y).zipAll((0 until canvas_size), y, y)).toArray

    pixels.par.foreach(
      (pix: (Int, Int)) => {
        val wall_target: RTTuple =
          Point(-half + pixel_size * pix._2, half - pixel_size * pix._1, wall_z)
        val ray: Ray = Ray(ray_origin, (wall_target - ray_origin).normalise())
        sphere.intersect(ray) match {
          case xs if xs.nonEmpty =>
            canvas.writePixel(
              pix._2,
              pix._1,
              Intersection
                .hit(xs)
                .shape
                .material
                .lighting(
                  Intersection.hit(xs).shape,
                  light,
                  ray.position(Intersection.hit(xs).t),
                  ray.direction.negate(),
                  Intersection.hit(xs).shape.normalAt(ray.position(Intersection.hit(xs).t)),
                  false
                )
            );
            Unit
          case _ => Unit
        }
      }
    ): Unit
    stringToFile("sphere3.ppm", canvas.toPPM)
  }

  def firstScene(): Unit = {
    val floorMaterial: Material =
      Material.defaultMaterial().setColour(Colour(1, 0.9, 0.9)).setSpecular(0)
    val floor: Sphere =
      Sphere.unitSphere().setTransform(Scaling(10, 0.01, 10)).setMaterial(floorMaterial)
    val left_wall: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(0, 0, 5)
        * RotationY(-math.Pi / 4) * RotationX(math.Pi / 2) * Scaling(10, 0.01, 10))
      .setMaterial(floorMaterial)
    val right_wall: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(0, 0, 5)
        * RotationY(math.Pi / 4) * RotationX(math.Pi / 2) * Scaling(10, 0.01, 10))
      .setMaterial(floorMaterial)

    val middleMaterial: Material =
      Material.defaultMaterial().setColour(Colour(0.1, 1, 0.5)).setDiffuse(0.7).setSpecular(0.3)
    val middleSphere: Sphere =
      Sphere.unitSphere().setTransform(Translation(-0.5, 1, 0.5)).setMaterial(middleMaterial)

    val rightMaterial: Material = middleMaterial.setColour(Colour(0.5, 1, 0.1))
    val rightSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(1.5, 0.5, -0.5) * Scaling(0.5, 0.5, 0.5))
      .setMaterial(rightMaterial)

    val leftMaterial: Material = middleMaterial.setColour(Colour(1, 0.8, 0.1))
    val leftSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(-1.5, 0.33, -0.75) * Scaling(0.33, 0.33, 0.33))
      .setMaterial(leftMaterial)

    val world: World = World(
      List(Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))),
      List(floor, leftSphere, rightSphere, middleSphere, left_wall, right_wall))

    val camera: Camera = Camera(800, 600, math.Pi / 3)
      .setTransform(viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene4.ppm", canvas.toPPM)

  }

  def planeScene(): Unit = {
    val floorMaterial: Material =
      Material.defaultMaterial().setColour(Colour(1, 0.9, 0.9)).setSpecular(0)
    val floor: Plane = Plane().setMaterial(floorMaterial)

    val middleMaterial: Material =
      Material.defaultMaterial().setColour(Colour(0.1, 1, 0.5)).setDiffuse(0.7).setSpecular(0.3)
    val middleSphere: Sphere =
      Sphere.unitSphere().setTransform(Translation(-0.5, 1, 0.5)).setMaterial(middleMaterial)

    val rightMaterial: Material = middleMaterial.setColour(Colour(0.5, 1, 0.1))
    val rightSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(1.5, 0.5, -0.5) * Scaling(0.5, 0.5, 0.5))
      .setMaterial(rightMaterial)

    val leftMaterial: Material = middleMaterial.setColour(Colour(1, 0.8, 0.1))
    val leftSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(-1.5, 0.33, -0.75) * Scaling(0.33, 0.33, 0.33))
      .setMaterial(leftMaterial)

    val world: World = World(List(Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))),
                             List(floor, leftSphere, rightSphere, middleSphere))

    val camera: Camera = Camera(800, 600, math.Pi / 3)
      .setTransform(viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene5.ppm", canvas.toPPM)
  }

  def stripeScene(): Unit = {
    val floorMaterial: Material = Material
      .defaultMaterial()
      .setColour(Colour(1, 0.9, 0.9))
      .setSpecular(0)
      .setPattern(StripePattern(Colour(0, 0, 0), Colour(1, 1, 1)))
    val floor: Plane = Plane().setMaterial(floorMaterial)

    val middleMaterial: Material = Material
      .defaultMaterial()
      .setColour(Colour(0.1, 1, 0.5))
      .setDiffuse(0.7)
      .setSpecular(0.3)
      .setPattern(
        StripePattern(Colour(0, 0, 0), Colour(1, 1, 1)).setTransform(RotationX(math.Pi / 2)))
    val middleSphere: Sphere =
      Sphere.unitSphere().setTransform(Translation(-0.5, 1, 0.5)).setMaterial(middleMaterial)

    val rightMaterial: Material = middleMaterial
      .setColour(Colour(0.5, 1, 0.1))
      .setPattern(
        StripePattern(Colour(0, 0, 0), Colour(1, 1, 1)).setTransform(RotationZ(math.Pi / 2)))
    val rightSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(1.5, 0.5, -0.5) * Scaling(0.5, 0.5, 0.5))
      .setMaterial(rightMaterial)

    val leftMaterial: Material = middleMaterial
      .setColour(Colour(1, 0.8, 0.1))
      .setPattern(
        StripePattern(Colour(0, 0, 0), Colour(1, 1, 1)).setTransform(RotationY(math.Pi / 2)))
    val leftSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(-1.5, 0.33, -0.75) * Scaling(0.33, 0.33, 0.33))
      .setMaterial(leftMaterial)

    val world: World = World(List(Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))),
                             List(floor, leftSphere, rightSphere, middleSphere))

    val camera: Camera = Camera(800, 600, math.Pi / 3)
      .setTransform(viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene6.ppm", canvas.toPPM)
  }

  def patternScene(): Unit = {
    val floorMaterial: Material = Material
      .defaultMaterial()
      .setSpecular(0)
      .setPattern(CheckeredPattern(Colour(0, 0.8, 0), Colour(1, 1, 1)))
    val floor: Plane = Plane().setMaterial(floorMaterial)

    val middleMaterial: Material = Material
      .defaultMaterial()
      .setColour(Colour(0.1, 1, 0.5))
      .setDiffuse(0.7)
      .setSpecular(0.3)
      .setPattern(GradientPattern(Colour(0, 0, 0), Colour(1, 1, 1)))
    val middleSphere: Sphere =
      Sphere.unitSphere().setTransform(Translation(-0.5, 1, 0.5)).setMaterial(middleMaterial)

    val rightMaterial: Material = middleMaterial
      .setColour(Colour(0.5, 1, 0.1))
      .setPattern(
        RingPattern(Colour(0, 0.7, 0), Colour(0, 0, 0.7)).setTransform(RotationZ(math.Pi / 2)))
    val rightSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(1.5, 0.5, -0.5) * Scaling(0.5, 0.5, 0.5))
      .setMaterial(rightMaterial)

    val leftMaterial: Material = middleMaterial
      .setColour(Colour(1, 0.8, 0.1))
      .setPattern(CheckeredPattern(Colour(0, 1, 0), Colour(1, 0, 1)))
    val leftSphere: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(-1.5, 0.33, -0.75) * Scaling(0.33, 0.33, 0.33))
      .setMaterial(leftMaterial)

    val world: World = World(List(Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))),
                             List(floor, leftSphere, rightSphere, middleSphere))

    val camera: Camera = Camera(800, 600, math.Pi / 3)
      .setTransform(viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene12.ppm", canvas.toPPM)
  }
  def reflectScene(): Unit = {
    val floorMaterial: Material = Material
      .defaultMaterial()
      .setSpecular(0)
      .setReflective(0.5)
      .setPattern(CheckeredPattern(Colour(0, 0.8, 0), Colour(1, 1, 1)))
    val wallMaterial: Material = Material.defaultMaterial().setSpecular(0)
    val floor: Plane           = Plane().setMaterial(floorMaterial)
    val left_wall: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(0, 0, 5)
        * RotationY(-math.Pi / 4) * RotationX(math.Pi / 2) * Scaling(10, 0.01, 10))
      .setMaterial(wallMaterial)
    val right_wall: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(0, 0, 5)
        * RotationY(math.Pi / 4) * RotationX(math.Pi / 2) * Scaling(10, 0.01, 10))
      .setMaterial(wallMaterial)

    val middleMaterial: Material = Material
      .defaultMaterial()
      .setColour(Colour(0.5, 0.5, 0.5))
      .setDiffuse(0.7)
      .setSpecular(0.3)
      .setReflective(1)
    val middleSphere: Sphere =
      Sphere.unitSphere().setTransform(Translation(-0.5, 1, 0.5)).setMaterial(middleMaterial)

    val world: World = World(List(Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))),
                             List(floor, middleSphere, left_wall, right_wall))

    val camera: Camera = Camera(800, 600, math.Pi / 3)
      .setTransform(viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene_reflect2.ppm", canvas.toPPM)

  }
  def refractScene(): Unit = {
    val floorMaterial: Material = Material
      .defaultMaterial()
      .setSpecular(0)
      .setReflective(0.5)
      .setPattern(CheckeredPattern(Colour(0, 0.8, 0), Colour(1, 1, 1)))
    val wallMaterial: Material = Material.defaultMaterial().setSpecular(0)
    val floor: Plane           = Plane().setMaterial(floorMaterial)
    val left_wall: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(0, 0, 5)
        * RotationY(-math.Pi / 4) * RotationX(math.Pi / 2) * Scaling(10, 0.01, 10))
      .setMaterial(wallMaterial)
    val right_wall: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(0, 0, 5)
        * RotationY(math.Pi / 4) * RotationX(math.Pi / 2) * Scaling(10, 0.01, 10))
      .setMaterial(wallMaterial)

    val middleMaterial: Material = Material
      .defaultMaterial()
      .setColour(Colour(0.5, 0.5, 0.5))
      .setDiffuse(0.7)
      .setSpecular(0.3)
      .setReflective(1)
    val middleSphere: Sphere =
      Sphere.unitSphere().setTransform(Translation(-0.5, 1, 0.5)).setMaterial(middleMaterial)

    val rightSphere: Sphere = Sphere
      .glassSphere()
      .setTransform(Translation(1.5, 0.5, -0.5) * Scaling(0.5, 0.5, 0.5))
      .setMaterial(Sphere.glassSphere().material.setColour(Colour(0.1, 0, 0)))

    val leftSphere: Sphere =
      Sphere.glassSphere().setTransform(Translation(-1.5, 0.33, -0.75) * Scaling(0.33, 0.33, 0.33))

    val world: World = World(
      List(Light.pointLight(Point(-10, 10, -10), Colour(1, 1, 1))),
      List(floor, middleSphere, left_wall, right_wall, leftSphere, rightSphere))

    val camera: Camera = Camera(400, 300, math.Pi / 3)
      .setTransform(viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vector(0, 1, 0)))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene_refract2.ppm", canvas.toPPM)

  }

  def refractScene2(): Unit = {
    val floorMaterial: Material = Material
      .defaultMaterial()
      .setSpecular(0.8)
      .setAmbient(0.5)
      .setDiffuse(0.4)
      .setReflective(0.1)
      .setPattern(CheckeredPattern(Colour(0, 0, 0), Colour(0.75, 0.75, 0.75)))
    val floor: Plane = Plane().setMaterial(floorMaterial).setTransform(RotationY(0.31415))

    val ceilingMaterial: Material = Material
      .defaultMaterial()
      .setSpecular(0)
      .setAmbient(0.5)
      .setPattern(CheckeredPattern(Colour(0.85, 0.85, 0.85), Colour(1, 1, 1))
        .setTransform(Scaling(0.2, 0.2, 0.2)))
    val ceiling: Plane = Plane()
      .setMaterial(ceilingMaterial)
      .setTransform(Translation(0, 5, 0))

    val wallMaterial: Material = Material
      .defaultMaterial()
      .setSpecular(0)
      .setPattern(CheckeredPattern(Colour(0, 0, 0), Colour(0.75, 0.75, 0.75))
        .setTransform(Scaling(0.5, 0.5, 0.5)))

    val west_wall: Plane = Plane()
      .setMaterial(wallMaterial)
      .setTransform(Translation(-5, 0, 0) * RotationZ(1.5708) * RotationY(1.5708))
    val east_wall: Plane = Plane()
      .setMaterial(wallMaterial)
      .setTransform(Translation(5, 0, 0) * RotationZ(1.5708) * RotationY(1.5708))

    val north_wall: Plane =
      Plane().setMaterial(wallMaterial).setTransform(Translation(0, 0, 5) * RotationX(1.5708))
    val south_wall: Plane =
      Plane().setMaterial(wallMaterial).setTransform(Translation(0, 0, -5) * RotationX(1.5708))

    val ball1: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(4, 1, 4))
      .setMaterial(
        Material
          .defaultMaterial()
          .setColour(Colour(0.8, 0.1, 0.3))
          .setSpecular(0)
      )

    val ball2: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(4.6, 0.4, 2.9) * Scaling(0.4, 0.4, 0.4))
      .setMaterial(
        Material
          .defaultMaterial()
          .setColour(Colour(0.1, 0.8, 0.2))
          .setShininess(200)
      )

    val ball3: Sphere = Sphere
      .unitSphere()
      .setTransform(Translation(2.6, 0.6, 4.4) * Scaling(0.6, 0.6, 0.6))
      .setMaterial(
        Material
          .defaultMaterial()
          .setColour(Colour(0.2, 0.1, 0.8))
          .setShininess(10)
          .setSpecular(0.4)
      )

    val glass_ball: Sphere = Sphere
      .glassSphere()
      .setTransform(Scaling(1, 1, 1) * Translation(0.25, 1, 0))
      .setMaterial(
        Material
          .glass()
          .setColour(Colour(0.8, 0.8, 0.9))
          .setAmbient(0)
          .setDiffuse(0.2)
          .setShininess(300)
          .setSpecular(0.9)
          .setTransparency(0.8)
          .setRefractiveIndex(1.57)
      )

    val world: World = World(List(Light.pointLight(Point(-4.9, 4.9, 1), Colour(1, 1, 1))),
                             List(glass_ball,
                                  ball1,
                                  ball2,
                                  ball3,
                                  floor,
                                  ceiling,
                                  north_wall,
                                  east_wall,
                                  south_wall,
                                  west_wall))
    val camera: Camera = Camera(1024, 768, 0.5)
      .setTransform(
        viewTransform(
          Point(-4.5, 0.85, -4),
          Point(0, 0.85, 0),
          Vector(0, 1, 0)
        ))

    val canvas: Canvas = camera.render(world)
    stringToFile("scene_refract12.ppm", canvas.toPPM)

  }

}
