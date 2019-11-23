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

class Material(val colour: Colour, val ambient: Double, val diffuse: Double,
               val specular: Double, val shininess: Double, val pattern: Option[Pattern],
               val reflective: Double, val transparency: Double, val refractiveIndex: Double) {

  final override def equals(that: Any): Boolean = {
    that match {
      case that: Material => (colour === that.colour) && doubleEq(ambient, that.ambient) &&
        doubleEq(diffuse, that.diffuse) && doubleEq(specular, that.specular) && doubleEq(shininess, that.shininess)
      case _ => false
    }
  }

  final def ===(that: Material): Boolean = {
    (colour === that.colour) && doubleEq(ambient, that.ambient) &&
      doubleEq(diffuse, that.diffuse) && doubleEq(specular, that.specular) && doubleEq(shininess, that.shininess)
  }


  final override def hashCode: Int = (colour, ambient, diffuse, specular, shininess).##

  def setColour(c: Colour): Material = new Material(c, ambient, diffuse, specular, shininess, pattern, reflective, transparency, refractiveIndex)

  def setAmbient(x: Double): Material = new Material(colour, x, diffuse, specular, shininess, pattern, reflective, transparency, refractiveIndex)

  def setDiffuse(x: Double): Material = new Material(colour, ambient, x, specular, shininess, pattern, reflective, transparency, refractiveIndex)

  def setSpecular(x: Double): Material = new Material(colour, ambient, diffuse, x, shininess, pattern, reflective, transparency, refractiveIndex)

  def setShininess(x: Double): Material = new Material(colour, ambient, diffuse, specular, x, pattern, reflective, transparency, refractiveIndex)

  def setPattern(x: Pattern): Material = new Material(colour, ambient, diffuse, specular, shininess, Some(x), reflective, transparency, refractiveIndex)

  def setReflective(x: Double): Material = new Material(colour, ambient, diffuse, specular, shininess, pattern, x, transparency, refractiveIndex)
  def setTransparency(x: Double): Material = new Material(colour, ambient, diffuse, specular, shininess, pattern, reflective, x, refractiveIndex)
  def setRefractiveIndex(x: Double): Material = new Material(colour, ambient, diffuse, specular, shininess, pattern, reflective, transparency, x)

  def ambientContribution(effective_colour: Colour): Colour = {
    effective_colour * ambient
  }

  def diffuseContribution(effective_colour: Colour, light_dot_normal: Double): Colour = {
    if (light_dot_normal < 0) Colour.black else effective_colour * diffuse * light_dot_normal
  }

  def specularContribution(light_dot_normal: Double, reflect_dot_eye: Double, light_intensity: Colour): Colour = {
    if (light_dot_normal < 0 || reflect_dot_eye <= 0) Colour.black else light_intensity * specular * math.pow(reflect_dot_eye, shininess)
  }

  def getColourFromPattern(obj: SpaceObject, p: RTTuple): Colour = {
    pattern match {
      case Some(x) => x.colourAtObject(obj, p)
      case None => colour
    }
  }

  def lighting(obj: SpaceObject, light: Light, p: RTTuple, eyev: RTTuple, normalv: RTTuple, in_shadow: Boolean): Colour = {
    val lightv: RTTuple = (light.position - p).normalise()
    val reflectv: RTTuple = lightv.negate().reflect(normalv)

    val useColour: Colour = getColourFromPattern(obj, p)
    val effective_colour: Colour = useColour ** light.intensity // Colour blend

    if (in_shadow) (ambientContribution(effective_colour)) else (
      ambientContribution(effective_colour) +
        diffuseContribution(effective_colour, lightv.dot(normalv)) +
        specularContribution(lightv.dot(normalv), reflectv.dot(eyev), light.intensity))

  }
}

object Material {
  def defaultMaterial(): Material = new Material(Colour(1, 1, 1), 0.1, 0.9, 0.9, 200.0, None, 0, transparency = 0.0, refractiveIndex = 1.0)
  def glass(): Material = Material.defaultMaterial().setTransparency(1.0).setRefractiveIndex(1.5)
}
