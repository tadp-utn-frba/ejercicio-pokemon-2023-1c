package ar.edu.utn.frba.tadp.pokemon

import scala.::
import scala.Option.option2Iterable
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.util.Try

// package object definido porque estamos en scala 2
object GimnasioPokemon {

  // descansar como metodo que produce efecto
  //  class Pokemon(var energia: Int, var energiaMaxima: Int) {
  //    def descansar(): Unit = {
  //      energia = energiaMaxima
  //    }
  //  }
  // descansar como metodo de consulta
  case class Pokemon(
                      energiaMaxima: Int,
                      experiencia: Int,
                      especie: Especie,
                      estado: Estado = Bien
                    )(energiaInicial: Int,
                      velocidadInicial: Int = 1,
                      fuerzaInicial: Int = 1) {

    val velocidad =
      velocidadInicial.max(1).min(100)
    val energia = this.energiaInicial.max(0)
    val fuerza =
      fuerzaInicial.max(1).min(100)

    def copiar(energiaMaxima: Int = energiaMaxima,
               experiencia: Int = experiencia,
               especie: Especie = especie,
               estado: Estado = estado,
               energia: Int = energia,
               velocidad: Int = velocidad,
               fuerza: Int = fuerza) =
      copy(energiaMaxima = energiaMaxima,
        experiencia = experiencia,
        especie = especie,
        estado = estado)(
        energiaInicial = energia,
        velocidadInicial = velocidad,
        fuerzaInicial = fuerza
      )

    def descansar(): Pokemon = cambiarEnergia(energiaMaxima)

    def nadar(minutos: Int): Try[Pokemon] = Try {
      estado match {
        case Bien if pierdeContra(Agua) => cambiarEstadoA(KO)
        case Bien =>
          val pokemonDespuesDeNadar = ganarExperiencia(200 * minutos).perderEnergia(minutos)
          if(tieneTipo(Agua)) pokemonDespuesDeNadar.ganarVelocidad(minutos / 60) else pokemonDespuesDeNadar
      }
    }

    def levantarPesas(kilos: Int): Try[Pokemon] = {
      Try {
        this match {
          case Fantasma() =>
            throw new RuntimeException("Un fantasma no puede levantar pesas")
          case pokemon if kilos > 10 * pokemon.fuerza =>
            pokemon.cambiarEnergia(energia - 10)
          case pokemon =>
            val experienciaPorLevantarPesas = if(tieneTipo(Pelea)) kilos * 2 else kilos
            pokemon.ganarExperiencia(experienciaPorLevantarPesas)
        }
      }
    }

    def tieneTipo(tipo: Tipo) = especie.tieneTipo(tipo)

    def pierdeContra(unTipo: Tipo) = especie.pierdeContra(unTipo)

    def cambiarEstadoA(nuevoEstado: Estado) = copiar(estado = nuevoEstado)

    def ganarVelocidad(cantidad: Int) = copiar(velocidad = velocidad + cantidad)
    def cambiarExperiencia(nuevaExperiencia: Int): Pokemon = copiar(experiencia = nuevaExperiencia)

    def ganarExperiencia(cantidad: Int) = copiar(experiencia = experiencia + cantidad)
    def perderEnergia(cantidad: Int) = copiar(energia = energia - cantidad)
    def cambiarEnergia(nuevaEnergia: Int): Pokemon = copiar(energia = nuevaEnergia)

    // que no tenga el metodo -> que no compile
    // que me devuelva al pokemon como esta -> no cumple el requerimiento
    // que tire una excepcion
    // que devuelva un valor de error (un Try)

  }

  trait Estado

  case object KO extends Estado

  case object Bien extends Estado

  object Especie {
    def unapply(pokemon: Pokemon) = Some(pokemon.especie)
  }

  case class Especie(tipoPrimario: Tipo,
                     tipoSecundario: Option[Tipo] = None) {

    val tipos = tipoPrimario :: tipoSecundario.toList

    def tieneTipo(unTipo: Tipo) =
      tipoPrimario == unTipo || tipoSecundario.contains(unTipo)

    def pierdeContra(unTipo: Tipo): Boolean =
      tipos.exists(tipo => tipo.pierdeContra(unTipo))

  }

  class Tipo(debilidades: => List[Tipo] = Nil) {
    def pierdeContra(otroTipo: Tipo): Boolean =
      debilidades.contains(otroTipo)

    def unapply(pokemon: Pokemon) = pokemon tieneTipo this
    // Otra opcion:
    //      Option.when(pokemon.tieneTipo(this))(pokemon)


    //    def unapply(pokemon: Pokemon) =
    //      if (pokemon.tieneTipo(this)) Option(pokemon) else None
  }

  case object Agua extends Tipo()

  case object Fuego extends Tipo(List(Agua))

  case object Tierra extends Tipo(List(Agua))

  case object Roca extends Tipo(List(Agua))

  case object Electricidad extends Tipo()

  case object Pelea extends Tipo()

  lazy val Fantasma: Tipo = new Tipo(List(Fantasma))
  // descansar como funcion
  //  def descansar(pokemon: Pokemon): Pokemon =
  //    new Pokemon(energia = energiaMaxima, energiaMaxima = energiaMaxima)


  def experienciaNecesariaParaNivel(nivel: Int, resistenciaEvolutiva: Int): Int = {
    nivel match {
      case 1 => 0
      case n => resistenciaEvolutiva + experienciaNecesariaParaNivel(n - 1, resistenciaEvolutiva) * 2
    }
  }

  class Actividad(accion: Pokemon => Try[Pokemon]) {
    def hacerse(pokemon: Pokemon): Try[Pokemon] =
      pokemon.estado match {
        case KO =>
          Try {
            throw new RuntimeException("Un pokemon no puede hacer una actividad si esta KO")
          }
        case Bien => accion(pokemon)
      }
  }

  val descansar: Actividad = new Actividad(pokemon => Try {
    pokemon.descansar()
  })

  def nadar(minutos: Int): Actividad = new Actividad(_.nadar(minutos))

  def levantarPesas(kilos: Int): Actividad = new Actividad(_.levantarPesas(kilos))
}