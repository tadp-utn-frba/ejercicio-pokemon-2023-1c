package ar.edu.utn.frba.tadp.pokemon

object Pokemon {
  def experienciaNecesariaParaNivel(nivel: Int, resistenciaEvolutiva: Int): Int = {
    nivel match {
      case 1 => 0
      case n => resistenciaEvolutiva + experienciaNecesariaParaNivel(n - 1, resistenciaEvolutiva) * 2
    }
  }
}