%% MATLAB code sections menu and keybindings
%
% Try:
%
% 1. Run (requires M-x matlab-shell, Unix only)
%      MATLAB -> Code Sections -> Run section
%      MATLAB -> Code Sections -> Run prior sections
% 2. Navigation
%      MATLAB -> Code Sections -> Backward section
%      MATLAB -> Code Sections -> Forward section
%      MATLAB -> Code Sections -> Move to beginning
%      MATLAB -> Code Sections -> Move to end
% 3. Move section content
%      MATLAB -> Code Sections -> Mark/select section
%      MATLAB -> Code Sections -> Move section up
%      MATLAB -> Code Sections -> Move section down
% 4. Super "Windows" key bindings can be enabled or disabled, see
%      MATLAB -> Code Sections -> Help

%% Preamble
clc
clear all

set(0,'defaultAxesTickLabelInterpreter','default');
set(0,'defaultTextInterpreter','latex');
set(0,'DefaultLegendInterpreter','latex');
set(0,'defaultAxesFontSize',13);

%% This is the first section, setting parameters

m = 1.0;
k = 4;
c = 2*0.01*sqrt(k*m);

F = 1.0;
Om = 4.0;

%% Second section - Transient Analysis
fsamp = 1024;
Tmax = 250*2*pi/Om;
Nt = fix(Tmax*fsamp);

[t,y] = ode45(@(t,y) [y(2); -c/m*y(2)-k/m*y(1)-F/m*cos(Om*t)], ...
              (0:Nt)*Tmax/Nt, [0;0]);

fsz = 14;
figure(1)
clf()
sp = stackedplot(t, y, 'DisplayLabels', {'y1','y2'});
xlabel('Time (s)')
set(gca, 'FontSize', fsz)
grid on;

%% Third section
N = 1000;
fsz = 14;
figure(2)
clf()
plot(eig(randn(N,N))/sqrt(N), '.'); hold on
plot(cos((0:100)*2*pi/100), sin((0:100)*2*pi/100), '-');
axis equal; grid on
set(gca, 'FontSize', fsz)
