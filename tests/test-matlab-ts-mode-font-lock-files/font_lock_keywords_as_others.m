% See https://github.com/acristoffers/tree-sitter-matlab/issues/73

arguments = 1
% break = 1
% case = 1
% catch = 1
% classdef = 1
% continue = 1
% else = 1
% elseif = 1
% end = 1
enumeration = 1
events = 1
% for = 1
% function = 1
% global = 1
% if = 1
methods = 1
% otherwise = 1
% parfor = 1
% persistent = 1
properties = 1
% return = 1
% spmd = 1
% switch = 1
% try = 1
% while = 1

% "keywords" allowed as struct fields and +DIR namespaces, e.g.
%   Create: ./+arguments/+break/+case/f.m
%   Containing:
%     function a=f
%         a=1;
%     end
%   Then:
%      v = arguments.break.case.f

s.arguments = 1
s.break = 1
s.case = 1
s.catch = 1
s.classdef = 1
s.continue = 1
s.else = 1
s.elseif = 1
s.end = 1
s.enumeration = 1
s.events = 1
s.for = 1
s.function = 1
s.global = 1
s.if = 1
s.methods = 1
s.otherwise = 1
s.parfor = 1
s.persistent = 1
s.properties = 1
s.return = 1
s.spmd = 1
s.switch = 1
s.try = 1
s.while = 1


