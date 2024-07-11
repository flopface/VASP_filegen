#!/usr/bin/env python3
 
import argparse
import os

POTCAR_PATHS = 'potcars'

def yay_or_neigh(prompt):
  yay = ['Y', 'Yes']
  neigh = ['N', 'No']
  while(True):
    answer = input(prompt).capitalize()
    if answer in yay: return True
    if answer in neigh: return False
    print('WARNING : input must be y or n')

def make_poscar(path, m_symbol, x_symbol, lattic_param, nx, ny, nz):

  atoms = 4*nx*ny*nz
  spaces_names = int(5-len(m_symbol))*' '
  spaces_nums = int(4-len(str(atoms)))*' '

  file_content = f'Auto generated rocksalt structure: {m_symbol}{x_symbol}\n1.0\n'\
                 f'        {lattic_param*nx:#.11g}        0.0000000000        0.0000000000\n'\
                 f'        0.0000000000        {lattic_param*ny:#.11g}        0.0000000000\n'\
                 f'        0.0000000000        0.0000000000        {lattic_param*nz:#.11g}\n'\
                 f'{m_symbol}{spaces_names}{x_symbol}\n'\
                 f'{atoms}{spaces_nums}{atoms}\nDirect\n'
  
  m_locs = ''
  x_locs = ''

  for i in range(2*nx):
    for j in range(2*ny):
      for k in range(2*nz):
        if (i + j + k) % 2 == 0:
          m_locs += f'        {i/(2*nx):.10f}        {j/(2*ny):.10f}        {k/(2*nz):.10f}        #{m_symbol}\n'
        else:
          x_locs += f'        {i/(2*nx):.10f}        {j/(2*ny):.10f}        {k/(2*nz):.10f}        #{x_symbol}\n'
  
  file_content = file_content + m_locs + x_locs

  with open(os.path.join(path, 'POSCAR'), 'w') as file:
    file.write(file_content)

def make_potcar(path, symbols):
  file_content = []
  for symbol in symbols:
    if os.path.isfile(os.path.join(POTCAR_PATHS, symbol)):
      with open(os.path.join(POTCAR_PATHS, symbol), 'r') as file:
        file_content += file.readlines()
        file_content[-1] += '\n'
    else:
      print(f'ERROR : No potcar for {symbol}')
      exit()
  
  with open(os.path.join(path, 'POTCAR'), 'w') as file:
    file.writelines(file_content)

def make_incar(path, )

def main(args):
  print(f'Chemical formula: {args.m_symbol}{args.x_symbol}\nLattice parameter: {args.lattic_param:.3f} \u212b\nCell size: {args.nx}x{args.ny}x{args.nz}')

  if args.directory:
    path = args.directory
    if os.path.isdir(args.directory): 
      if not yay_or_neigh(f'WARNING : Directory \'{args.directory}\' already exists, overwrite? (y/n) : '): exit()
    else: os.makedirs(args.directory)

  else:
    path = ''
    for file in ['POSCAR', 'INCAR', 'POTCAR', 'run', 'OUTCAR', 'myJob.err', 'myJob.out', 'out.log']:
      if os.path.isfile(file): 
        if yay_or_neigh(f'WARNING : File \'{file}\' already exists, overwrite? (y/n) : '): os.remove(file)
        else: exit()

  make_potcar(path, [args.m_symbol, args.x_symbol])
  make_poscar(path, args.m_symbol, args.x_symbol, args.lattic_param, args.nx, args.ny, args.nz)
 
if __name__ == '__main__':
 
  parser = argparse.ArgumentParser(description = 'This script is the one true creator (of VASP input files) and you will treat it as such')
 
  parser.add_argument('m_symbol', type = str, help = 'The chemical symbol of the metal, eg Ti')
  parser.add_argument('x_symbol', type = str, help = 'The chemical symbol of the non-metal, eg C')
  parser.add_argument('lattic_param', type = float, help = 'Distance between M atoms')
  parser.add_argument('nx', type = int, help = 'Number of unit cells in x direction')
  parser.add_argument('ny', type = int, help = 'Number of unit cells in y direction')
  parser.add_argument('nz', type = int, help = 'Number of unit cells in z direction')
  
  parser.add_argument('-dir', '--directory', type = str, help = 'The directory for the input files to be stored in')
  parser.add_argument('-encut', '--encut', type = int, help = 'The highest energy considered in the electron pseudo-orbital expansion')
  parser.add_argument('-kspacing', '--kspacing', type = float, help = 'The spacing between sample points in reciprocal space')
 
  args = parser.parse_args()
  main(args)