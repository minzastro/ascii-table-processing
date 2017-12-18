#!/usr/bin/env python2
import numpy as np
import argparse
import sys
import matplotlib.pyplot as plt

color_sequence = 'rbgmcy'

parser = argparse.ArgumentParser(description="""
Plot data from the input.
""", formatter_class=argparse.RawDescriptionHelpFormatter)

parser.add_argument('-x', type=str, default='1,2',
                    help='Columns to use')

parser.add_argument('-i', type=str, default=None,
                    help='Input file')

parser.add_argument('-t', '--title', type=str, default=None,
                    help='Plot title')

parser.add_argument('-d', '--delimiter', type=str, default=None,
                    help='Input file')

parser.add_argument('-s', '--style', type=str, default='-',
                    help='Plot style')

parser.add_argument('--save', type=str, default=None,
                    help='Output file name')

parser.add_argument('-l', '--log', type=str, default='',
                    help='x or y or xy for logscale')

parser.add_argument('--header', action="store_true",
                    default=False,
                    help='use header')

parser.add_argument('-lp', '--legend_position', type=str,
                    default='lower center',
                    help='legend position or none for no legend')

args = parser.parse_args()

if args.i is None:
    source = sys.stdin
else:
    source = open(args.i, 'r')

header = None
if args.header:
    if args.delimiter is not None:
        header = source.readline().strip().split(args.delimiter)
    else:
        header = source.readline().strip().split()
    skiprows = 1
else:
    skiprows = 0


if args.delimiter is None:
    data = np.loadtxt(source, skiprows=skiprows)
else:
    data = np.loadtxt(source, delimiter=args.delimiter, skiprows=skiprows)

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

if args.title is not None:
    plt.title(args.title)
else:
    plt.title(args.i)

if args.log == 'x':
    plt.semilogx()
elif args.log == 'y':
    plt.semilogy()
elif args.log == 'xy':
    plt.loglog()

labels = []
for j, column in enumerate(columns[1:]):
    plt.plot(data[:, columns[0]-1], data[:, column-1], get_style(j))
    if args.header:
        labels.append(r'$%s$' % header[column - 1])
        plt.xlabel(header[columns[0] - 1])
    else:
        labels.append('Column %s' % (column - 1))
    if args.legend_position != 'none':
        plt.legend(labels, loc=args.legend_position,
                   fancybox=True, shadow=True, ncol=2)

if args.save is None:
    plt.show()
else:
    plt.savefig(args.save, format='png')
