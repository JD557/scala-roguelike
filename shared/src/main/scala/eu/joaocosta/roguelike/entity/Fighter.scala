package eu.joaocosta.roguelike.entity

case class Fighter(hp: Int, maxHp: Int, attack: Int, defense: Int) {
  val isDead: Boolean      = hp <= 0
  val statusString: String = s"HP: $hp/$maxHp | ATK: $attack | DEF: $defense"
  def computeDamage(that: Fighter): Int =
    math.min(math.max(0, this.attack - that.defense), that.hp)
  def computeDamage(that: Option[Fighter]): Int =
    that.fold(0)(computeDamage)
  def applyDamage(damage: Int): Fighter =
    copy(hp = math.max(0, hp - damage))
}
