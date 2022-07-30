package eu.joaocosta.roguelike

import eu.joaocosta.minart.input.KeyboardInput

case class Input(
    keyPresses: Set[KeyboardInput.Key] = Set.empty,
    repeatCountdown: Map[KeyboardInput.Key, Int] = Map.empty
) {
  def update(keyInput: KeyboardInput): Input = {
    val (repeatPresses, newCountdown) =
      keyInput.keysDown
        .map(key =>
          key -> (
            if (keyInput.keysPressed(key)) constants.keyFirstRepeat
            else repeatCountdown.view.mapValues(_ - 1).getOrElse(key, constants.keyNextRepeat)
          )
        )
        .toMap
        .partition(_._2 == 0)
    Input(
      keyPresses = keyInput.keysPressed ++ repeatPresses.keySet,
      repeatCountdown = newCountdown
    )
  }
}
