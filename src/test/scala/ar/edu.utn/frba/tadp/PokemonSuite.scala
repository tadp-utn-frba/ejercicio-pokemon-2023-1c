package ar.edu.utn.frba.tadp.pokemon

import ar.edu.utn.frba.tadp.pokemon.Pokemon.experienciaNecesariaParaNivel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PokemonSpec extends AnyFlatSpec {
  "la experiencia necesaria estar en nivel 1" should "ser 0" in {
    val resistenciaEvolutiva = 1

    assert(experienciaNecesariaParaNivel(1, resistenciaEvolutiva) === 0)
  }

  "la experiencia necesaria para estar en un nivel" should "ser el doble de la necesaria para el nivel anterior + la resistencia evolutiva" in {
    val resistenciaEvolutiva = 350

    assert(experienciaNecesariaParaNivel(3, resistenciaEvolutiva) === 1050)
  }
}
