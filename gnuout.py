#!/usr/bin/python
import numpy as np
import argparse
import matplotlib
import matplotlib.pyplot as plt

color_sequence = 'rbgmcy'

parser = argparse.ArgumentParser(description="""
Plot data from the input.
""", formatter_class=argparse.RawDescriptionHelpFormatter)

parser.add_argument('-x', type=str, default='1,2',
                    help='Columns to use')

parser.add_argument('-i', type=str, default=None,
                    help='Input file')

parser.add_argument('-d', '--delimiter', type=str, default=None,
                    help='Input file')

parser.add_argument('-s', '--style', type=str, default='-',
                    help='Plot style')

parser.add_argument('--save', type=str, default=None,
                    help='Output file name')

parser.add_argument('-l', '--log', type=str, default='',
                    help='x or y or xy for logscale')


args = parser.parse_args()

if args.delimiter == None:
    data = np.loadtxt(args.i)
else:
    data = np.loadtxt(args.i, delimiter=args.delimiter)


def get_color(icolor):
    while icolor >= len(color_sequence):
        icolor = icolor - len(color_sequence)
    return color_sequence[icolor]

def get_style(icolor):
    return '%s%s' % (get_color(icolor), args.style)

def convert_column(column):
    # TODO: Add verification here.
    if '-' in column:
        low_column, up_column = column.split('-')
    else:
        low_column, up_column = column, column
    low_column = int(low_column)
    up_column = int(up_column)
    return range(low_column, up_column + 1)

columns = []
for colpar in args.x.split(','):
    columns.extend(convert_column(colpar))

plt.title(args.i)
if args.log == 'x':
    plt.semilogx()
elif args.log == 'y':
    plt.semilogy()
elif args.log == 'xy':
    plt.loglog()

for j, column in enumerate(columns[1:]):
    plt.plot(data[:, columns[0]-1], data[:, column-1], get_style(j))

if args.save == None:
    plt.show()
else:
    plt.savefig(args.save, format='png')
