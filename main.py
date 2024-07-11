#!/usr/bin/env python3
 
import argparse
import os

def make_poscar(path, m_symbol, x_symbol, bond_length, nx, ny, nz):

  file_content = f''

  with open(os.path.join(path, 'POSCAR'), 'w') as file:
    print('hi')

 
def main(args):
  print(f'Chemical formula: {args.m_symbol}{args.x_symbol}\nBond length: {args.bond_length:.3f} \u212b\nCell size: {args.nx}x{args.ny}x{args.nz}')
  if args.encut: print(f'ENCUT = {args.encut}')
  if args.kspacing: print(f'KSPACING = {args.kspacing}')
 
  print(os.getcwd())
 
if __name__ == '__main__':
 
  parser = argparse.ArgumentParser(description = 'This script is the one true creator (of VASP input files) and you will treat it as such')
 
  parser.add_argument('m_symbol', type = str, help = 'The chemical symbol of the metal, eg Ti')
  parser.add_argument('x_symbol', type = str, help = 'The chemical symbol of the non-metal, eg C')
  parser.add_argument('bond_length', type = float, help = 'Distance between M and X atoms')
  parser.add_argument('nx', type = int, help = 'Number of unit cells in x direction')
  parser.add_argument('ny', type = int, help = 'Number of unit cells in y direction')
  parser.add_argument('nz', type = int, help = 'Number of unit cells in z direction')
 
  parser.add_argument('-encut', '--encut', type = int, help = 'The highest energy considered in the electron pseudo-orbital expansion')
  parser.add_argument('-kspacing', '--kspacing', type = float, help = 'The spacing between sample points in reciprocal space')
 
  args = parser.parse_args()
  main(args)