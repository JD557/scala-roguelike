package eu.joaocosta.roguelike

case class Level(
    playerStart: Entity.Player,
    gameMap: GameMap,
    npcs: List[Entity.Npc]
) {
  def isWalkable(x: Int, y: Int) =
    gameMap.tiles.get((x, y)).forall(_.walkable) && !npcs.exists(npc => x == npc.x && y == npc.y)
}
