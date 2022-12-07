import { readFileSync } from 'fs'
import path from 'path'

const input = readFileSync(path.join(__dirname, 'input.txt'), 'utf8')
  .split('\n')
  .map((x: string) =>
    x.split(' ').map((x) => {
      if (x == 'X') return 'A'
      if (x == 'Y') return 'B'
      if (x == 'Z') return 'C'

			return x
    })
  )
  .slice(0, -1)

const moves = ['A', 'B', 'C'] as const
type Moves = typeof moves[number]

const getDistance = (x: Moves, y: Moves) => {
  return (moves.indexOf(x) - moves.indexOf(y) + moves.length) % moves.length
}

const getResultScore = (x: Moves, y: Moves) => {
  const distance = getDistance(x, y)
  return ((distance + 1) % 3) * 3
}

const getMoveScore = (x: Moves) => {
  switch (x) {
    case 'A':
      return 1
    case 'B':
      return 2
    case 'C':
      return 3
  }
}

const fin = input.reduce((agg: number, [a, b]) => {
  return agg + getResultScore(b, a) + getMoveScore(b)
}, 0)

console.log(`Total score: ${fin}`)
