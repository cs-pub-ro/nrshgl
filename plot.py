import matplotlib.pyplot as plt
from decimal import *
import numpy as np
import glob
import os
import seaborn as sns
from bitstring import BitArray
import matplotlib.colors as colors
from mpl_toolkits.axes_grid1 import make_axes_locatable
import math

def additionPlot():
    #info dict
    results = {
        "Morris" : {},
        "MorrisHEB" : {},
        "MorrisUnaryHEB" : {},
        "MorrisBiasHEB" : {},
        "OldPosit" : {},
        "Brainfloat" : {},
        "IEEE754" : {},
        "Tensorfloat" : {},
        "FP24" : {},
        "PXR24" : {},
        "Posit" : {}
    }
    for dataType in results:
        results[dataType] = {
            'size' : [],
            'luts' : [],
            'dsps' : [],
            'power' : [],
            'speed' : []
        }
    #read the information for N-Body
    rootdir = "results"
    print(glob.glob(os.path.join(rootdir, '*.txt')))
    #read the info for low magnitude numbers
    for file_path in glob.glob(os.path.join(rootdir, '*.txt')):
        filename =  os.path.basename(file_path).split(".")[0]
        if(filename == "addition"):
            with open(os.path.join(os.getcwd(), file_path), 'r') as f: # open in readonly mode
                for row in f:
                    tokens = row.split(" ")
                    dataType = tokens[0].strip()
                    size = int(tokens[1].strip())
                    power = float(tokens[2].strip())
                    luts = int(tokens[3].strip())
                    dsps = int(tokens[4].strip())
                    speed = float(tokens[-1].strip())
                    results[dataType]['size'].append(size)
                    results[dataType]['power'].append(power)
                    results[dataType]['luts'].append(luts)
                    results[dataType]['dsps'].append(dsps)
                    results[dataType]['speed'].append(speed)
    print(results)
    fig = plt.figure(figsize=(20,20))
    ax0 = fig.add_subplot(2, 2, 1)
    ax1 = fig.add_subplot(2, 2, 2, sharex = ax0)
    ax2 = fig.add_subplot(2, 2, 3, sharex = ax1)
    ax3 = fig.add_subplot(2, 2, 4, sharex = ax2)
    ax0.set_xlabel ('Size', fontsize=18)
    ax1.set_xlabel ('Size', fontsize=18)
    ax2.set_xlabel ('Size', fontsize=18)
    ax3.set_xlabel ('Size', fontsize=18)
    ax0.set_ylabel("Max Frecuency (Mhz)", fontsize = 18)
    ax1.set_ylabel("LUTs", fontsize = 18)
    ax2.set_ylabel("Power Consumption (W)", fontsize = 18)
    ax3.set_ylabel("DSPs", fontsize = 18)
    plt.rcParams.update({'font.size': 18})
    markers = {
        "Morris" : "s",
        "MorrisHEB" : "o",
        "MorrisUnaryHEB" : "v",
        "MorrisBiasHEB" : "^",
        "OldPosit" : "x",
        "IEEE754" : "s",
        "Posit" : "o",
        "PXR24" : "2",
        "Tensorfloat" : "<",
        "Brainfloat" : ">",
        "FP24" : "8",
    }
    linestyles = {
        "Morris" : "-",
        "MorrisHEB" : "-",
        "MorrisUnaryHEB" : "-",
        "MorrisBiasHEB" : "-",
        "OldPosit" : "-",
        "IEEE754" : ":",
        "Posit" : ":",
        "PXR24" : ":",
        "Tensorfloat" : ":",
        "Brainfloat" : "-.",
        "FP24" : "-.",
    }
    for dataType in results:
        ax0.plot(results[dataType]['size'], results[dataType]['speed'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
        ax1.plot(results[dataType]['size'], results[dataType]['luts'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
        ax2.plot(results[dataType]['size'], results[dataType]['power'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
        ax3.plot(results[dataType]['size'], results[dataType]['dsps'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
    #box = ax0.get_position()
    #ax0.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    #box = ax1.get_position()
    #ax1.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    box = ax2.get_position()
    ax2.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    box = ax3.get_position()
    ax3.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])

    # Put a legend below current axis
    ax2.legend(loc='upper center', bbox_to_anchor=(1.1, -0.1), fancybox=True, shadow=True, ncol=6, fontsize=18)
    image_format = 'pdf' # e.g .png, .svg, etc.
    fig_name = "addition" + ".pdf"
    image_name = fig_name
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def additionPlot2():
    #info dict
    results = {
        "Morris" : {},
        "MorrisHEB" : {},
        "MorrisUnaryHEB" : {},
        "MorrisBiasHEB" : {},
        "OldPosit" : {},
        "Brainfloat" : {},
        "IEEE754" : {},
        "Tensorfloat" : {},
        "FP24" : {},
        "PXR24" : {},
        "Posit" : {}
    }
    for dataType in results:
        results[dataType] = {
            'size' : [],
            'luts' : [],
            'dsps' : [],
            'power' : [],
            'speed' : []
        }
    #read the information for N-Body
    rootdir = "results"
    print(glob.glob(os.path.join(rootdir, '*.txt')))
    #read the info for low magnitude numbers
    for file_path in glob.glob(os.path.join(rootdir, '*.txt')):
        filename =  os.path.basename(file_path).split(".")[0]
        if(filename == "addition"):
            with open(os.path.join(os.getcwd(), file_path), 'r') as f: # open in readonly mode
                for row in f:
                    tokens = row.split(" ")
                    dataType = tokens[0].strip()
                    size = int(tokens[1].strip())
                    power = float(tokens[2].strip())
                    luts = int(tokens[3].strip())
                    dsps = int(tokens[4].strip())
                    speed = float(tokens[-1].strip())
                    results[dataType]['size'].append(size)
                    results[dataType]['power'].append(power)
                    results[dataType]['luts'].append(luts)
                    results[dataType]['dsps'].append(dsps)
                    results[dataType]['speed'].append(speed)
    print(results)
    rows =2
    cols = 2
    plt.rcParams.update({'font.size': 24})
    fig, ax = plt.subplots(rows, cols, sharex=False, sharey=False, figsize=(34,20))
    ax[0, 0].set_ylabel("Max Frecuency (Mhz)", fontsize = 18)
    ax[0, 1].set_ylabel("LUTs", fontsize = 18)
    ax[1, 0].set_ylabel("Power Consumption (W)", fontsize = 18)
    ax[1, 1].set_ylabel("DSPs", fontsize = 18)
    plt.rcParams.update({'font.size': 18})
    markers = {
        "Morris" : "s",
        "MorrisHEB" : "o",
        "MorrisUnaryHEB" : "v",
        "MorrisBiasHEB" : "^",
        "OldPosit" : "x",
        "IEEE754" : "s",
        "Posit" : "o",
        "PXR24" : "2",
        "Tensorfloat" : "<",
        "Brainfloat" : ">",
        "FP24" : "8",
    }
    linestyles = {
        "Morris" : "-",
        "MorrisHEB" : "-",
        "MorrisUnaryHEB" : "-",
        "MorrisBiasHEB" : "-",
        "OldPosit" : "-",
        "IEEE754" : ":",
        "Posit" : ":",
        "PXR24" : ":",
        "Tensorfloat" : ":",
        "Brainfloat" : "-.",
        "FP24" : "-.",
    }
    for dataType in results:
        ax[0, 0].plot(results[dataType]['size'], results[dataType]['speed'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
        ax[0, 1].plot(results[dataType]['size'], results[dataType]['luts'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
        ax[1, 0].plot(results[dataType]['size'], results[dataType]['power'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
        ax[1, 1].plot(results[dataType]['size'], results[dataType]['dsps'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType])
    #box = ax0.get_position()
    #ax0.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    #box = ax1.get_position()
    #ax1.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    #box = ax2.get_position()
    #ax2.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])
    #box = ax3.get_position()
    #ax3.set_position([box.x0, box.y0 + box.height * 0.05, box.width, box.height * 0.9])

    # Put a legend below current axis
    #ax2.legend(loc='upper center', bbox_to_anchor=(1.1, -0.1), fancybox=True, shadow=True, ncol=6, fontsize=18)

    def forward(x):
        return np.log2(x)
    def inverse(x):
        return 2**x
    # settings the limits in 32 bits
    for index in range(0,rows):
        for jindex in range(0,cols):
            ax[index, jindex].set_xlim([8, 64])
            ax[index, jindex].set_xscale('function', functions=(forward, inverse))
            # amximum accuracy 57.01
            #ax[index, jindex].set_ylim([0, 0.6])
            # from the highest precision to the lowest
            ax[index, jindex].set_xticks([8, 16, 19, 24, 32, 64])
            #ax[index, jindex].set_yticks([0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6])
            ax[index, jindex].set_xlabel("Size (Bits)", fontsize=18)
            ax[index, jindex].grid()
            # the maximum accuracy - accuracy treshold
            #ax[index, jindex].hlines(y=0.56, xmin=32, xmax=0, linewidth=2, color='black')
    ax[1, 1].set_ylim([0,16])
    #ax[1, 1].set_yscale('function', functions=(forward, inverse))
    ax[1, 1].set_yticks([1,2,4,8,16])
    #legend
    #divider = make_axes_locatable(ax[0, 3])
    #cax = divider.append_axes("bottom", size="5%", pad=0.05)
    legend = ax[0, 0].legend(loc='lower right', bbox_to_anchor=(0.5, -0.7), fancybox=True, shadow=True, ncol=6, fontsize=24)
    handles, labels = ax[0, 0].get_legend_handles_labels()
    legend.remove()
    fig.legend(handles, labels, loc='lower center', bbox_to_anchor=(0.5, -0.02), fancybox=True, shadow=True, ncol=6, fontsize=24)
    image_format = 'pdf' # e.g .png, .svg, etc.
    fig_name = "addition" + ".pdf"
    image_name = fig_name
    plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)


def generalPlot():
    #read the information for N-Body
    rootdir = "results"
    print(glob.glob(os.path.join(rootdir, '*.txt')))
    rows =2
    cols = 2
    plt.rcParams.update({'font.size': 24})
    def forward(x):
        return np.log2(x)
    def inverse(x):
        return 2**x
    markers = {
        "Morris" : "s",
        "MorrisHEB" : "o",
        "MorrisUnaryHEB" : "v",
        "MorrisBiasHEB" : "^",
        "OldPosit" : "x",
        "IEEE754" : "s",
        "Posit" : "o",
        "PXR24" : "2",
        "Tensorfloat" : "<",
        "Brainfloat" : ">",
        "FP24" : "8",
        "GFPU" : "4",
    }
    linestyles = {
        "Morris" : "-",
        "MorrisHEB" : "-",
        "MorrisUnaryHEB" : "-",
        "MorrisBiasHEB" : "-",
        "OldPosit" : "-",
        "IEEE754" : ":",
        "Posit" : ":",
        "PXR24" : ":",
        "Tensorfloat" : ":",
        "Brainfloat" : "-.",
        "FP24" : "-.",
        "GFPU" : "-.",
    }
    #read the info for low magnitude numbers
    for file_path in glob.glob(os.path.join(rootdir, '*.txt')):
        filename =  os.path.basename(file_path).split(".")[0]
        #info dict
        results = {
            "Morris" : {},
            "MorrisHEB" : {},
            "MorrisUnaryHEB" : {},
            "MorrisBiasHEB" : {},
            "OldPosit" : {},
            "Brainfloat" : {},
            "IEEE754" : {},
            "Tensorfloat" : {},
            "FP24" : {},
            "PXR24" : {},
            "Posit" : {},
            "GFPU" : {}
        }
        for dataType in results:
            results[dataType] = {
                'size' : [],
                'luts' : [],
                'dsps' : [],
                'power' : [],
                'speed' : []
            }
        with open(os.path.join(os.getcwd(), file_path), 'r') as f: # open in readonly mode
            for row in f:
                tokens = row.split(" ")
                dataType = tokens[0].strip()
                size = int(tokens[1].strip())
                power = float(tokens[2].strip())
                luts = int(tokens[3].strip())
                dsps = int(tokens[4].strip())
                speed = float(tokens[-1].strip())
                results[dataType]['size'].append(size)
                results[dataType]['power'].append(power)
                results[dataType]['luts'].append(luts)
                results[dataType]['dsps'].append(dsps)
                results[dataType]['speed'].append(speed)
        fig, ax = plt.subplots(rows, cols, sharex=False, sharey=False, figsize=(34,20))
        ax[0, 0].set_ylabel("Max Frecuency (Mhz)", fontsize = 18)
        ax[0, 1].set_ylabel("Power Consumption (W)", fontsize = 18)
        ax[1, 0].set_ylabel("LUTs", fontsize = 18)
        ax[1, 1].set_ylabel("DSPs", fontsize = 18)
        if len(results['GFPU']['size']) == 0:
            del results['GFPU']
        for dataType in results:
            ax[0, 0].plot(results[dataType]['size'], results[dataType]['speed'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)
            ax[0, 1].plot(results[dataType]['size'], results[dataType]['power'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)
            ax[1, 0].plot(results[dataType]['size'], results[dataType]['luts'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)
            ax[1, 1].plot(results[dataType]['size'], results[dataType]['dsps'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)

        # settings the limits in 32 bits
        for index in range(0,rows):
            for jindex in range(0,cols):
                ax[index, jindex].set_xlim([8, 64])
                ax[index, jindex].set_xscale('function', functions=(forward, inverse))
                # amximum accuracy 57.01
                #ax[index, jindex].set_ylim([0, 0.6])
                # from the highest precision to the lowest
                ax[index, jindex].set_xticks([8, 16, 19, 24, 32, 64])
                #ax[index, jindex].set_yticks([0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6])
                ax[index, jindex].set_xlabel("Size (Bits)", fontsize=18)
                ax[index, jindex].grid()
                # the maximum accuracy - accuracy treshold
                #ax[index, jindex].hlines(y=0.56, xmin=32, xmax=0, linewidth=2, color='black')
        #ax[1, 1].set_ylim([0,16])
        #ax[1, 1].set_yscale('function', functions=(forward, inverse))
        #ax[1, 1].set_yticks([1,2,4,8,16])
        ax[1, 0].set_yscale('log')
        #legend
        #divider = make_axes_locatable(ax[0, 3])
        #cax = divider.append_axes("bottom", size="5%", pad=0.05)
        legend = ax[0, 0].legend(loc='lower right', bbox_to_anchor=(0.5, -0.7), fancybox=True, shadow=True, ncol=6, fontsize=24)
        handles, labels = ax[0, 0].get_legend_handles_labels()
        legend.remove()
        fig.legend(handles, labels, loc='lower center', bbox_to_anchor=(0.5, -0.02), fancybox=True, shadow=True, ncol=6, fontsize=24)
        image_format = 'pdf' # e.g .png, .svg, etc.
        fig_name = "hgl_" + filename + ".pdf"
        image_name = fig_name
        plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)

def statsPlot():
    #read the information for N-Body
    rootdir = "results"
    print(glob.glob(os.path.join(rootdir, '*.txt')))
    rows =2
    cols = 2
    plt.rcParams.update({'font.size': 24})
    def forward(x):
        return np.log2(x)
    def inverse(x):
        return 2**x
    markers = {
        "Morris" : "s",
        "MorrisHEB" : "o",
        "MorrisUnaryHEB" : "v",
        "MorrisBiasHEB" : "^",
        "OldPosit" : "x",
        "IEEE754" : "s",
        "Posit" : "o",
        "PXR24" : "2",
        "Tensorfloat" : "<",
        "Brainfloat" : ">",
        "FP24" : "8",
        "GFPU" : "4",
    }
    linestyles = {
        "Morris" : "-",
        "MorrisHEB" : "-",
        "MorrisUnaryHEB" : "-",
        "MorrisBiasHEB" : "-",
        "OldPosit" : "-",
        "IEEE754" : ":",
        "Posit" : ":",
        "PXR24" : ":",
        "Tensorfloat" : ":",
        "Brainfloat" : "-.",
        "FP24" : "-.",
        "GFPU" : "-.",
    }
    #read the info for low magnitude numbers
    for file_path in glob.glob(os.path.join(rootdir, '*.txt')):
        filename =  os.path.basename(file_path).split(".")[0]
        if(not ('stats' in filename)):
            continue
        #info dict
        results = {
            "Morris" : {},
            "MorrisHEB" : {},
            "MorrisUnaryHEB" : {},
            "MorrisBiasHEB" : {},
            "OldPosit" : {},
            "Brainfloat" : {},
            "IEEE754" : {},
            "Tensorfloat" : {},
            "FP24" : {},
            "PXR24" : {},
            "Posit" : {},
            "GFPU" : {}
        }
        for dataType in results:
            results[dataType] = {
                'size' : [],
                'es' : [],
                'fs' : [],
                'as' : [],
                'afs' : []
            }
        with open(os.path.join(os.getcwd(), file_path), 'r') as f: # open in readonly mode
            for row in f:
                tokens = row.split(" ")
                dataType = tokens[0].strip()
                size = int(tokens[1].strip())
                power = int(tokens[2].strip())
                luts = int(tokens[3].strip())
                dsps = int(tokens[4].strip())
                speed = int(tokens[-1].strip())
                results[dataType]['size'].append(size)
                results[dataType]['es'].append(power)
                results[dataType]['fs'].append(luts)
                results[dataType]['as'].append(dsps)
                results[dataType]['afs'].append(speed)
        fig, ax = plt.subplots(rows, cols, sharex=False, sharey=False, figsize=(34,20))
        ax[0, 0].set_ylabel("Exponent size", fontsize = 18)
        ax[0, 1].set_ylabel("Fraction size", fontsize = 18)
        ax[1, 0].set_ylabel("Accumulator size", fontsize = 18)
        ax[1, 1].set_ylabel("Accumulator fraction size", fontsize = 18)
        if len(results['GFPU']['size']) == 0:
            del results['GFPU']
        for dataType in results:
            ax[0, 0].plot(results[dataType]['size'], results[dataType]['es'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)
            ax[0, 1].plot(results[dataType]['size'], results[dataType]['fs'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)
            ax[1, 0].plot(results[dataType]['size'], results[dataType]['as'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)
            ax[1, 1].plot(results[dataType]['size'], results[dataType]['afs'], label = dataType, marker=markers[dataType], linestyle=linestyles[dataType], markersize=12)

        # settings the limits in 32 bits
        for index in range(0,rows):
            for jindex in range(0,cols):
                ax[index, jindex].set_xlim([8, 64])
                ax[index, jindex].set_xscale('function', functions=(forward, inverse))
                # amximum accuracy 57.01
                #ax[index, jindex].set_ylim([0, 0.6])
                # from the highest precision to the lowest
                ax[index, jindex].set_xticks([8, 16, 19, 24, 32, 64])
                #ax[index, jindex].set_yticks([0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6])
                ax[index, jindex].set_xlabel("Size (Bits)", fontsize=18)
                ax[index, jindex].grid()
                # the maximum accuracy - accuracy treshold
                #ax[index, jindex].hlines(y=0.56, xmin=32, xmax=0, linewidth=2, color='black')
        #ax[1, 1].set_ylim([0,16])
        #ax[1, 1].set_yscale('function', functions=(forward, inverse))
        #ax[1, 1].set_yticks([1,2,4,8,16])
        ax[1, 0].set_yscale('log')
        ax[1, 1].set_yscale('log')
        ax[1, 0].axhline(y=2048, color="black")
        ax[1, 1].axhline(y=1024, color="black")
        #legend
        #divider = make_axes_locatable(ax[0, 3])
        #cax = divider.append_axes("bottom", size="5%", pad=0.05)
        legend = ax[0, 0].legend(loc='lower right', bbox_to_anchor=(0.5, -0.7), fancybox=True, shadow=True, ncol=6, fontsize=24)
        handles, labels = ax[0, 0].get_legend_handles_labels()
        legend.remove()
        fig.legend(handles, labels, loc='lower center', bbox_to_anchor=(0.5, -0.02), fancybox=True, shadow=True, ncol=6, fontsize=24)
        image_format = 'pdf' # e.g .png, .svg, etc.
        fig_name = "hgl_" + filename + ".pdf"
        image_name = fig_name
        plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200)

def socPlot():
    #read the information for N-Body
    rootdir = "results"
    print(glob.glob(os.path.join(rootdir, '*.txt')))
    rows =2
    cols = 2
    plt.rcParams.update({'font.size': 24})
    #read the info for low magnitude numbers
    for file_path in glob.glob(os.path.join(rootdir, '*.txt')):
        filename =  os.path.basename(file_path).split(".")[0]
        if(not ('soc' in filename)):
            continue
        #info dict
        experiments = {
            "pi(Leibniz)" : {},
            "pi(Nilakantha)" : {},
            "e(Euler)" : {},
            "sin(1)" : {},
            "MM" : {},
            "KNN" : {},
            "LR" : {},
            "NB" : {},
            "CT" : {},
            "CNN" : {}
        }
        for experimentType in experiments:
            experiments[experimentType] = {
                'size' : [],
                'speedup' : []
            }
        with open(os.path.join(os.getcwd(), file_path), 'r') as f: # open in readonly mode
            for row in f:
                tokens = row.split(" ")
                experimentType = tokens[0].strip()
                size = int(tokens[1].strip())
                speedup = float(tokens[2].strip())
                experiments[experimentType]['size'].append(size)
                experiments[experimentType]['speedup'].append(speedup)
        layersNo = np.fromiter((1 for x in experiments), dtype='i8').sum()
        # create de figure with 1 rows and 1 cols
        rows = 1
        cols = 1
        fig, ax = plt.subplots(rows, cols, sharex=False, sharey=False, figsize=(34,20))
        image_format = 'pdf' # e.g .png, .svg, etc.
        # image
        colors = [
            'red',
            'green',
            'blue',
            'cyan',
            'chocolate',
            'yellow',
            'salmon',
            'purple'
        ]
        palettes = [
            'Purples',
            'Blues',
            'Greens',
            'Oranges',
            'Reds',
            'Purples',
            'Blues',
            'Reds'
        ]
        nrss = [
            'OldPosit 8',
            'OldPosit 16',
            'OldPosit 32'
        ]
        # 6 NRSs so a space left a space right
        barWidth = 1/5
        # the place on the barplot for x-axis
        # barwidth distance between them
        br = {}
        br[0] = np.arange(layersNo)
        for nrs_index in range(1,3):
            br[nrs_index] = [x + barWidth for x in br[nrs_index-1]]

        # draw the barplot and for the systems with 2 attribute write the first
        # attribute on the bar
        for nrs_index in range(0,3):
            values = np.fromiter((experiments[experimentType]['speedup'][nrs_index] for experimentType in experiments), dtype='d')
            ax.bar(br[nrs_index], values, align='center', width = barWidth, edgecolor = colors[nrs_index], color = colors[nrs_index], label = nrss[nrs_index])

        # rotate the x label a little
        ax.set_xticks([r + barWidth for r in range(layersNo)],
                (experimentType for experimentType in experiments), rotation=30, ha='right')
        # set the bits limit
        # low precision now more than 16 bits
        ax.set_yscale('symlog', linthresh=1.3)
        ax.set_ylim([0, 7])
        # set the important values
        ax.set_yticks([0, 1, 1.1, 1.2, 1.3])
        # the y-label
        ax.set_ylabel("Speedup")
        # set the grid to see better the important values
        ax.grid(axis='y')
        # the legent in the free space on the graph
        ax.legend(loc="upper left",  ncol=6, shadow=True, title="Legend", fancybox=True)

        #save the image
        image_format = 'pdf' # e.g .png, .svg, etc.
        fig_name = "hgl_" + filename + ".pdf"
        image_name = fig_name
        plt.savefig(image_name, format=image_format, bbox_inches='tight', pad_inches = 0, dpi=1200) 
   
#additionPlot()
#additionPlot2()
#generalPlot()
#statsPlot()
socPlot()