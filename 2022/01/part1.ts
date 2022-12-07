import { readFileSync } from 'fs'
import path from 'path'

const input = readFileSync(path.join(__dirname, 'input.txt'), "utf8")
const values = input
  .split('\n\n')
  .map((x: string) => x.split('\n').map((x) => parseInt(x)))

const chonk = values.reduce((agg: number, x: number[]) => {
  const sum = x.reduce((agg, x) => agg + x)

  return sum > agg ? sum : agg
}, -Infinity)
console.log(`Max elf is: ${chonk}`)
