#!/usr/bin/env python3
 
import argparse
import os
 
POTCAR_PATHS = 'potcars'
DEFAULT_INCAR = 'standard_incar'

STANDARD_RWIGS = {'V' : 1.34, 'N' : 0.75, 'C' : 0.77, 'Ti' : 1}

ALLOWED_INPUTS = {'m_symbol' : 'any',
                  'x_symbol' : 'any',
                  'lattic_param' : 'positive',
                  'nx' : 'positive',
                  'ny' : 'positive',
                  'nz' : 'positive',
                  'nodes' : (1, 32),
                  'time' : (0, 48),
                  'cores' : (1, 48),
                  'dir' : 'any',
                  'f' : 'any',
                  'system' : 'any',
                  'kspacing' : 'positive',
                  'kgamma' : 'bool',
                  'encut' : 'positive',
                  'ibrion' : [-1, 0, 1, 2, 3, 5, 6, 7, 8, 40, 44],
                  'isif' : [0, 1, 2, 3, 4, 5, 6, 7, 8],
                  'nsw' : 'positive',
                  'ediff' : 'positive',
                  'ediffg' : 'any',
                  'prec' : ['Normal', ' Single', ' SingleN', ' Accurate', ' Low', ' Medium', ' High'],
                  'lreal' : 'bool',
                  'ispin' : [1, 2],
                  'istart' : [0, 1, 2, 3],
                  'lorbit' : [0, 1, 2, 5, 10, 11, 12],
                  'ismear' : [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
                  'sigma' : 'positive',
                  'isym' : [-1, 0, 1, 2, 3],
                  'algo' : ['Normal', 'VeryFast', 'Fast', 'Conjugate', 'All', 'Damped', 'Subrot', 'Eigenval', 'Exact', 'None', 'Nothing', 'CHI', 'G0W0', 'GW0', 'GW', 'scGW0', 'scGW', 'G0W0R', 'GW0R', 'GWR', 'scGW0R', 'scGWR', 'ACFDT', 'RPA', 'ACFDTR', 'RPAR', 'BSE', 'TDHF'],
                  'rwigs' : 'any'}

def yay_or_neigh(prompt):
  yay = ['Y', 'Yes']
  neigh = ['N', 'No']
  while(True):
    answer = input(prompt).capitalize()
    if answer in yay: 
      print()
      return True
    if answer in neigh: 
      print()
      return False
    print('WARNING : input must be y or n')

def validate_input(name, input, allowed):
  if input in allowed: return
  print_str = f'ERROR : {name} must be either: '
  for option in allowed[:-1]: print_str += f'{option}, '
  print_str = print_str[:-2] + f' or {allowed[-1]}\n'
  print(print_str)
  exit()

def validate_range(name, input, range):
  if (input >= range[0]) and (input <= range[1]): return
  print(f'{name} must be between {range[0]} and {range[1]}')
  exit()

def validate_bool(name, input):
  validate_input(name, input, ['.TRUE.', '.FALSE.', 'True', 'False', 'true', 'false', 't', 'f', '1', '0'])
  if input in ['True', 'true', 't', '1']: return '.TRUE.'
  else: return '.FALSE'

def validate_args(args):
  arg_list = [[arg[0], arg[1]] for arg in list(vars(args).items()) if arg[1] != None]
  for arg in arg_list:
    if arg[1] != None:
      allowed = ALLOWED_INPUTS[arg[0]]
      if type(allowed) == type([1, 2, 3]):
        validate_input(arg[0], arg[1], allowed)
      elif type(allowed) == type((1, 2, 3)):
        validate_range(arg[0], arg[1], allowed)
      elif allowed != 'any': 
        if allowed == 'positive': 
          if arg[1] < 0:
            print(f'{arg[0]} must be positive')
            exit()
        elif allowed == 'bool':
          arg[1] = validate_bool(arg[0], arg[1])
  return arg_list
  
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

def modify_incar(file_name, parameter, new_value):
  with open(file_name, 'r') as file:
    lines = file.readlines()
    for i in range(len(lines)):
      if parameter.upper() in lines[i]:
        if type(new_value) == type(False):
          print(new_value)
          if new_value == True: lines[i] = lines[i][:lines[i].find('=')+2] + '.TRUE.\n'
          else: lines[i] = lines[i][:lines[i].find('=')+2] + '.FALSE.\n'
        elif type(new_value) == type([1,2,3]):
          lines[i] = lines[i][:lines[i].find('=')+1]
          for val in new_value: lines[i] = lines[i] + f' {val}'
          lines[i] = lines[i] + '\n'
        else: lines[i] = lines[i][:lines[i].find('=')+2] + f'{new_value}\n'
    
  with open(file_name, 'w') as file: file.writelines(lines)

def make_incar(path, arg_list):

  file_name = os.path.join(path, 'INCAR')

  with open(DEFAULT_INCAR, 'r') as file: lines = file.readlines()
  with open(file_name, 'w') as file: file.writelines(lines)

  i = 0
  while arg_list[i][0] != 'f': i += 1
  while i < len(arg_list) - 1:
    i += 1
    if arg_list[i][1] != None: modify_incar(file_name, *arg_list[i])

def make_run(path, nodes, cores, time):
  if not cores: cores = 48
  hour_str = f'{int(time // 1)}'
  min_str = f'{int(((time % 1) * 60) // 1)}'
  sec_str = f'{int((((time % 1) * 60) % 1) * 60)}'
  while(len(hour_str) < 2): hour_str = '0' + hour_str
  while(len(min_str) < 2): min_str = '0' + min_str
  while(len(sec_str) < 2): sec_str = '0' + sec_str

  time_str = f"{hour_str}:{min_str}:{sec_str}"
  file_content = '#!/bin/bash\n'\
                f'#SBATCH --nodes={nodes}\n'\
                f'#SBATCH --ntasks-per-node={cores}\n'\
                f'#SBATCH --time={hour_str}:{min_str}:{sec_str}\n'\
                 '#SBATCH --error=myJob.err\n'\
                 '#SBATCH --output=myJob.out\n'\
                 '#SBATCH --account=FUA38_FISEAMIV\n'\
                 '#SBATCH --partition=skl_fua_prod\n'\
                 'module load ... vasp?\n'\
                 'srun vasp'
  
  with open(os.path.join(path, 'run'), 'w') as file: file.write(file_content)
  


 
def main(args):
  #print(f'Chemical formula: {args.m_symbol}{args.x_symbol}\nLattice parameter: {args.lattic_param:.3f} \u212b\nCell size: {args.nx}x{args.ny}x{args.nz}')
  print()
  arg_list = validate_args(args)
  if args.dir:
    path = args.dir
    if os.path.isdir(args.dir) and not args.f:
      if not yay_or_neigh(f'WARNING : Directory \'{args.dir}\' already exists, overwrite? (y/n) : '): exit()
    else: os.makedirs(args.dir, exist_ok = True)
 
  elif not args.f:
    path = ''
    for file in ['POSCAR', 'INCAR', 'POTCAR', 'run', 'OUTCAR', 'myJob.err', 'myJob.out', 'out.log']:
      if os.path.isfile(file):
        if yay_or_neigh(f'WARNING : File \'{file}\' already exists, overwrite? (y/n) : '): os.remove(file)
        else: exit()

  make_incar(path, arg_list)
  make_potcar(path, [args.m_symbol, args.x_symbol])
  make_poscar(path, args.m_symbol, args.x_symbol, args.lattic_param, args.nx, args.ny, args.nz)
  make_run(path, args.nodes, args.cores, args.time)
 
if __name__ == '__main__':
 
  parser = argparse.ArgumentParser(description = 'This script is the one true creator (of VASP input files) and you will treat it as such')
 
  parser.add_argument('m_symbol', type = str, help = 'The chemical symbol of the metal, eg Ti')
  parser.add_argument('x_symbol', type = str, help = 'The chemical symbol of the non-metal, eg C')
  parser.add_argument('lattic_param', type = float, help = 'Distance between M atoms')
  parser.add_argument('nx', type = int, help = 'Number of unit cells in x direction')
  parser.add_argument('ny', type = int, help = 'Number of unit cells in y direction')
  parser.add_argument('nz', type = int, help = 'Number of unit cells in z direction')
  parser.add_argument('nodes', type = int, help = 'Number of nodes to run the calculation on')
  parser.add_argument('time', type = float, help = 'Allocated CPU time in hours')
  
  parser.add_argument('-cores', type = int, help = 'CPUs per node')
  parser.add_argument('-dir', type = str, help = 'The directory for the input files to be stored in')
  parser.add_argument("-f", action = "store_true", help = "Force any directory or file overrides")
  
  parser.add_argument('-system', type = str, help = 'The name of the system')
  parser.add_argument('-kspacing', type = float, help = 'The spacing between sample points in reciprocal space')
  parser.add_argument('-kgamma', type = str,  help = 'Whether the kpoints center on the Gamma point')
  parser.add_argument('-encut', type = int,   help = 'The highest energy considered in the electron pseudo-orbital expansion')
  parser.add_argument('-ibrion', type = int,   help = 'Ionic relaxation algorithm')
  parser.add_argument('-isif', type = int,   help = 'Degree of freedom for ion relaxation')
  parser.add_argument('-nsw', type = int,   help = 'Number of ionic steps')
  parser.add_argument('-ediff', type = float, help = 'Stopping criterion for electronic self-consistency')
  parser.add_argument('-ediffg', type = float, help = 'Stopping criterion for ionic relaxation')
  parser.add_argument('-prec', type = str,   help = 'Precision of the calculations')
  parser.add_argument('-lreal', type = str,  help = 'Whether to use real-space projections for the augmentation charges')
  parser.add_argument('-ispin', type = int,   help = 'Spin polarization')
  parser.add_argument('-istart', type = int,   help = 'Starting job type')
  parser.add_argument('-lorbit', type = int,   help = 'Write out DOSCAR and PROCAR')
  parser.add_argument('-ismear', type = int,   help = 'Smearing method')
  parser.add_argument('-sigma', type = float, help = 'Smearing width')
  parser.add_argument('-isym', type = int,   help = 'Symmetry operations')
  parser.add_argument('-algo', type = str,   help = 'Algorithm for electronic minimization')
  parser.add_argument('-rwigs', type = float, help = 'Wigner-Seitz radii for each atom type', nargs='+')
  
  args = parser.parse_args()

  main(args)