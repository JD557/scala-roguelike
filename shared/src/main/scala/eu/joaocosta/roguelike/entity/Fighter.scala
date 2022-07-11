package eu.joaocosta.roguelike.entity

case class Fighter(hp: Int, maxHp: Int, attack: Int, defense: Int, attackVerb: String) {
  val isDead: Boolean = hp <= 0
  def computeDamage(that: Fighter): Int =
    math.min(math.max(0, this.attack - that.defense), that.hp)
  def updateHp(dHp: Int): Fighter =
    copy(hp = math.min(math.max(0, hp + dHp), maxHp))

  def applyDamage(damage: Int): Fighter =
    updateHp(-damage)
  def heal(amount: Int): Fighter =
    updateHp(amount)
}

object Fighter {
  trait Component[E <: Entity with Fighter.Component[E]] {
    def fighter: Fighter
    def updateFighter(f: Fighter => Fighter): E
    def applyDamage(damage: Int): E = updateFighter(_.applyDamage(damage))
    def heal(amount: Int): E        = updateFighter(_.heal(amount))
  }
}
