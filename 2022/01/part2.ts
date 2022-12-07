import { readFileSync } from 'fs'
import path from 'path'

const input = readFileSync(path.join(__dirname, 'input.txt'), 'utf8')
const values = input
  .split('\n\n')
  .map((x: string) => x.split('\n').map((x) => parseInt(x)))

const chonk = values
  .reduce(
    (agg: number[], x: number[]) => {
      const sum = x.reduce((agg, x) => agg + x)

      const i = agg.reduce((j, y, k) => (sum > y ? (j == -1 ? k : j) : -1), -1)

      return i != -1
        ? [...agg.slice(0, i), sum, ...agg.slice(i)].slice(0, 3)
        : agg
    },
    [-Infinity, -Infinity, -Infinity]
  )
  .reduce((agg: number, x: number) => agg + x)
console.log(`Max elf is: ${chonk}`)
