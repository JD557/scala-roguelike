package eu.joaocosta.roguelike.entity

case class Fighter(hp: Int, maxHp: Int, attack: Int, defense: Int) {
  val isDead: Boolean      = hp <= 0
  val statusString: String = s"HP: $hp/$maxHp | ATK: $attack | DEF: $defense"
  def computeDamage(that: Fighter): Int =
    math.min(math.max(0, this.attack - that.defense), that.hp)
  def applyDamage(damage: Int): Fighter =
    copy(hp = math.max(0, hp - damage))
}

object Fighter {
  trait Component[E <: Entity] {
    def fighter: Fighter
    def updateFighter(f: Fighter => Fighter): E
    def applyDamage(damage: Int): E = updateFighter(_.applyDamage(damage))
  }
}
