% -*- matlab-ts -*-

outFile = 'out.txt';
lines = ['line1\n', 'line2\n'];

writelines(lines, outFile, LineEnding = '\n');
