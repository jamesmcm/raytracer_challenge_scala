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
  if (args.length < 2 || args.length > 2) {
    println("Usage: sbt run scene.yaml output.ppm")
    System.exit(1)
  }
  val (cam: Camera, w: World) = YAMLScene.parseYAMLToScene(args(0))
  val canvas: Canvas          = cam.render(w)
  stringToFile(args(1), canvas.toPPM)

}
