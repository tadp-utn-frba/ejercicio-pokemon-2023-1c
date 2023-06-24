package ar.edu.utn.frba.tadp.pokemon

import ar.edu.utn.frba.tadp.pokemon.GimnasioPokemon._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Success
class PokemonSpec extends AnyFlatSpec {
  val especieCharmander = Especie(tipoPrimario = Fuego)
  val especiePoliwhirl = Especie(tipoPrimario = Agua)
  val especiePikachu = Especie(tipoPrimario = Electricidad)
  val especieMankey = Especie(tipoPrimario = Pelea)

  "la experiencia necesaria estar en nivel 1" should "ser 0" in {
    val resistenciaEvolutiva = 1

    assert(experienciaNecesariaParaNivel(1, resistenciaEvolutiva) === 0)
  }

  "la experiencia necesaria para estar en un nivel" should "ser el doble de la necesaria para el nivel anterior + la resistencia evolutiva" in {
    val resistenciaEvolutiva = 350

    assert(experienciaNecesariaParaNivel(3, resistenciaEvolutiva) === 1050)
  }

  "descansar" should "llevar la energia de un pokemon a su energia maxima" in {
    val charmander = Pokemon(
      energiaMaxima = 100,
      especie = especieCharmander,
      experiencia = 0,
    )(energiaInicial = 10)

    assert(charmander.descansar().energia === 100)
  }

  "si un pokemon que no es de agua nada" should "disminuir en 1 punto la energia y aumentar en 200 la experiencia por cada minuto nadado" in {
    val pikachu = Pokemon(
      energiaMaxima = 100,
      especie = especiePikachu,
      experiencia = 0
    )(energiaInicial = 10)
    val pikachuEntrenado = pikachu.nadar(2)

    assert(pikachuEntrenado.energia === 8)
    assert(pikachuEntrenado.experiencia === 400)
  }

  //TODO: si un pokemon nada mas de lo que le da la energia, deberia aumentar su experiencia tanto como realmente pudo nadar

  "la energia de un pokemon" should "no ser negativa" in {
    val pikachu = Pokemon(
      energiaMaxima = 100,
      especie = especiePikachu,
      experiencia = 0
    )(energiaInicial = -5)

    assert(pikachu.energia === 0)
  }



  "si un pokemon de tipo agua nada" should "gana 1 punto de velocidad ademas de disminuir su energia y aumentar su experiencia como los demas" in {
    val poliwhirl = Pokemon(
      energiaMaxima = 300,
      especie = especiePoliwhirl,
      experiencia = 0
    )(energiaInicial = 200,
      velocidadInicial = 1,
      fuerzaInicial = 1)

    val poliwhirlEntrenado = poliwhirl.nadar(120)

    assert(poliwhirlEntrenado.energia === 80)
    assert(poliwhirlEntrenado.experiencia === 24000)
    assert(poliwhirlEntrenado.velocidad === 3)
  }

  "si un pokemon levanta pesas" should "gana 1 punto de experiencia por cada kilo levantado" in {
    val poliwhirl = Pokemon(
      energiaMaxima = 300,
      especie = especiePoliwhirl,
      experiencia = 0
    )(energiaInicial = 200,
      velocidadInicial = 1,
      fuerzaInicial = 1)

    val Success(poliwhirlEntrenado) = poliwhirl.levantarPesas(5)

    assert(poliwhirlEntrenado.experiencia === 5)
    assert(poliwhirlEntrenado.energia === poliwhirl.energia)

  }

  "si un pokemon levanta mas de 10 kilos por cada punto de fuerza" should "no gana nada de experiencia y pierde 10 de energia" in {
    val poliwhirl = Pokemon(
      energiaMaxima = 300,
      especie = especiePoliwhirl,
      experiencia = 0
    )(energiaInicial = 200,
      velocidadInicial = 1,
      fuerzaInicial = 1)

    val Success(poliwhirlEntrenado) = poliwhirl.levantarPesas(20)

    assert(poliwhirlEntrenado.experiencia === poliwhirl.experiencia)
    assert(poliwhirlEntrenado.energia === 190)
  }

  "los pokemones de tipo pelea al levantar pesas" should "ganan el doble de puntos de experiencia que el resto" in {
    val mankey = Pokemon(
      energiaMaxima = 300,
      especie = especieMankey,
      experiencia = 0
    )(energiaInicial = 200)
    val poliwhirl = mankey.copy(especie = especiePoliwhirl)(energiaInicial = 200,
      velocidadInicial = mankey.velocidad,
      fuerzaInicial = mankey.fuerza)

    val Success(mankeyEntrenado) = mankey.levantarPesas(5)
    val Success(poliwhirlEntrenado) = poliwhirl.levantarPesas(5)

    assert(mankeyEntrenado.experiencia === poliwhirlEntrenado.experiencia * 2)
  }
}
