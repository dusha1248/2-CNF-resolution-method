
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND IMPLY LEFT NAME NOT OR RIGHTexpression : NAME\n                  | NOT NAMEexpression : LEFT expression RIGHTexpression : expression AND expressionexpression : two IMPLY twoexpression : two OR twotwo : NAME\n           | NOT NAME'
    
_lr_action_items = {'NAME':([0,3,4,6,9,10,15,],[2,7,2,2,14,14,17,]),'NOT':([0,4,6,9,10,],[3,3,3,15,15,]),'LEFT':([0,4,6,],[4,4,4,]),'$end':([1,2,7,11,12,13,14,16,17,],[0,-1,-2,-4,-3,-5,-7,-6,-8,]),'AND':([1,2,7,8,11,12,13,14,16,17,],[6,-1,-2,6,6,-3,-5,-7,-6,-8,]),'RIGHT':([2,7,8,11,12,13,14,16,17,],[-1,-2,12,-4,-3,-5,-7,-6,-8,]),'IMPLY':([2,5,7,],[-7,9,-8,]),'OR':([2,5,7,],[-7,10,-8,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'expression':([0,4,6,],[1,8,11,]),'two':([0,4,6,9,10,],[5,5,5,13,16,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expression","S'",1,None,None,None),
  ('expression -> NAME','expression',1,'p_expression','Resolution_method.py',61),
  ('expression -> NOT NAME','expression',2,'p_expression','Resolution_method.py',62),
  ('expression -> LEFT expression RIGHT','expression',3,'p_expression_group','Resolution_method.py',72),
  ('expression -> expression AND expression','expression',3,'p_expression_and','Resolution_method.py',77),
  ('expression -> two IMPLY two','expression',3,'p_expression_imply','Resolution_method.py',81),
  ('expression -> two OR two','expression',3,'p_expression_or','Resolution_method.py',89),
  ('two -> NAME','two',1,'p_two','Resolution_method.py',97),
  ('two -> NOT NAME','two',2,'p_two','Resolution_method.py',98),
]
