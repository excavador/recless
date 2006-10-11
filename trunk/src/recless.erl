%% @title Recless: A Type Inferring Erlang Parse Transform
%% @author Yariv Sadan (yarivvv@gmail.com, http://yarivsblog.com)
%%
%% @doc Recless is a type inferring Erlang parse transform.
%%  Instead of writing these lines:
%%
%%     City = ((Project#project.owner)#owner.address)#address.city,
%% 
%%     NewProject =
%%       Project#project{owner =
%%        (Project#project.owner)#owner{address =
%%          ((Project#project.owner)#owner.address)#address{city =
%%            'Boston'}}}.
%%
%%  it lets your write these lines:
%%
%%     City = Project.owner.address.city,
%%     NewProject = Project.owner.address.city = 'Boston'.
%%
%%
%% WARNING: RECLESS IS HIGHLY EXPERIMENTAL, AND IT'S NOT FINISHED.
%% USE AT YOUR OWN RISK.

%% Copyright (c) 2006 Yariv Sadan
%% 
%% Permission is hereby granted, free of charge, to any person obtaining
%% a copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to 
%% permit persons to whom the Software is furnished to do so, subject to 
%% the following conditions:
%%
%% The above copyright notice and this permission notice shall be included 
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY 
%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, 
%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE 
%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(recless).
-author("Yariv Sadan (yarivvv@gmail.com, http://yarivsblog.com)").
-export([parse_transform/2]).
-define(L(Val), io:fwrite("~b ~p~n~n", [?LINE, Val])).


%% the irony... 
-record(context, {module,
		  func_stack = [],
		  trees = gb_trees:empty(),
		  var_types = gb_trees:empty(),
		  rec_types = gb_trees:empty(),
		  func_types = gb_trees:empty()}).

add_tree(Module, Tree, Context) ->
    Context#context{trees =
		    gb_trees:enter(Module, Tree, Context#context.trees)}.

get_tree(Module, Context) ->
    gb_trees:lookup(Module, Context#context.trees).

add_var_type(VarName, Type, Context) ->
    Context#context{var_types =
		gb_trees:enter(VarName, Type,
			       Context#context.var_types)}.

get_var_type(VarName, Context) ->
    gb_trees:lookup(VarName, Context#context.var_types).
    
add_rec_type(RecName, Type, Context) ->
    Context#context{rec_types =
		gb_trees:enter(RecName, Type,
			       Context#context.rec_types)}.

get_rec_type(RecName, Context) ->
    gb_trees:lookup(RecName, Context#context.rec_types).

add_func_type(Module, FuncName, Arity, Form, Type, Context) ->
    Context#context{func_types =
		gb_trees:enter({Module, FuncName, Arity}, {Form, Type},
			       Context#context.func_types)}.

get_func_type(Name, Arity, Context) ->
    [{Module, _FuncName1, _Arity1} | _] = Context#context.func_stack,
    get_func_type(Module, Name, Arity, Context).

get_func_type(Module, FuncName, Arity, Context) ->
    gb_trees:lookup({Module, FuncName, Arity}, Context#context.func_types).

push_func(Module, FuncName, Arity, Context) ->
    Context#context{func_stack =
		    [{Module, FuncName, Arity} | Context#context.func_stack]}.

pop_func(Context) ->
    case Context#context.func_stack of
	[] -> Context;
	[_First | Rest] -> Context#context{func_stack = Rest}
    end.

parse_transform(Tree, _Options) ->
    Module = get_module(Tree),
    parse_tree(Tree, [], add_tree(Module, Tree,
				  #context{module = Module})).

parse_tree([], DestTree, _Context) -> lists:reverse(DestTree);
parse_tree([Form | Rest], DestTree, Context) ->
    case Form of 
	{attribute,_,record,{Name, Fields}} = Rec ->
	    {FieldTypes, _Context1} =
		get_types(Fields, Context),
	    RecType = {record_type, Name, FieldTypes},
	    parse_tree(
	      Rest, [Rec|DestTree], add_rec_type(Name, RecType, Context));
	{function,L,Name,Arity,Clauses} ->
	    Module = Context#context.module,
	    {Form1, Context1} =
		infer_func(Module, L, Name, Arity, Clauses, Context),
	    parse_tree(Rest, [Form1|DestTree], Context1);
	Other ->
	    parse_tree(Rest, [Other|DestTree], Context)
    end.

get_module(Tree) ->
    {value, {attribute,_,module,Module}} = lists:keysearch(module, 3, Tree),
    Module.

infer_func(Name, Arity, Context) ->
    [{Module, _LastName, _LastArity} | _] = Context#context.func_stack,
    case lists:any(
	   fun({Module1, Name1, Arity1}) when Module1 == module,
					      Name == Name1,
					      Arity == Arity1 -> true;
	      (_Other) -> false
	   end, Context#context.func_stack) of
	true -> recursive;
	false -> infer_func(Module, Name, Arity, Context)
    end.

infer_func(Module, Name, Arity, Context) ->
    case get_func_type(Module, Name, Arity, Context) of
	none ->
	    {Tree1, Context1} = 
		case get_tree(Module, Context) of
		    none ->
			%% this code path is not yet tested
			exit(error),

			[_,_,_,{compile,[_,_,_,{source, SrcPath}]}] =
			    Module:module_info(),
			case epp:parse_file(SrcPath, [], []) of
			    {ok, Tree} ->
				{Tree, add_tree(Module, Tree, Context)};
			    Err -> exit(Err)
			end;
		    {value, Tree} -> {Tree, Context}
		end,
	    infer_func(Module, Name, Arity, Tree1, Context1);
	{value, {Form, _Type}} ->
	    {Form, Context}
    end.

infer_func(Module, Name, Arity, [], _Context) ->
    exit({no_such_func, {Module, Name, Arity}});
infer_func(Module, Name, Arity,
	   [{function,Line,Name,Arity,Clauses} | _], Context) ->
    infer_func(Module, Line, Name, Arity, Clauses, Context);
infer_func(Module, Name, Arity, [_Form | Rest], Context) ->
    infer_func(Module, Name, Arity, Rest, Context).


infer_func(Module, Line, Name, Arity, Clauses, Context) ->
    case get_func_type(Module, Name, Arity, Context) of
	none ->
	    Context1 = push_func(Module, Name, Arity, Context),
	    {NewClauses, FuncType, Context2} =
		infer(Clauses, Context1),
	    FuncForm = {function,Line,Name,Arity,NewClauses},
	    Context3 = pop_func(Context2),
	    {FuncForm,
	     add_func_type(
	       Module, Name, Arity, FuncForm, FuncType, Context3)};
	{value, {Form, _Type}} ->
	    {Form, Context}
    end.

infer(Clauses, Context) when is_list(Clauses) ->
    {NewClauses, {FinalBlockType, Context3}} =
	lists:mapfoldl(
	  fun(Clause, {BlockType, Context1}) ->
		  {NewClause, LastExprType, Context2} =
		      infer(Clause, Context1),
		  NewBlockType =
		      if LastExprType == recursive ->
			      BlockType;
			 true ->
			      case BlockType of
				  undefined ->
				      {defined, LastExprType};
				  {defined, LastExprType} = Type ->
				      Type;
				  {defined, _Other} ->
				      ambiguous;
				  ambiguous ->
				      ambiguous
			      end
		      end,
		  {NewClause, {NewBlockType, Context2}}
	  end, {undefined, Context}, Clauses),

    FinalBlockType1 =
	case FinalBlockType of
	    {defined, Type} ->
		Type;
	    _ ->
		FinalBlockType
	end,
    {NewClauses, FinalBlockType1, Context3};

infer({clause, Line, Params, Guards, Exprs}, Context) ->
    Context1 = pattern(Params, Context),
    {Exprs2, LastExprType, Context2} = exprs(Exprs, Context1, last),
    {{clause, Line, Params, Guards, Exprs2}, LastExprType,
     Context2}.

pattern(Patterns, Context) when is_list(Patterns) ->
    lists:foldl(
      fun(P1, Acc) ->
	      pattern(P1, Acc)
      end,
      Context, Patterns);
pattern(Pattern, Context) ->
    case Pattern of
	{match,_,P1, P2} ->
	    match(P1, P2, Context);
	{tuple, _, Elems} ->
	    pattern(Elems, Context);
	{cons,_,P1,P2} ->
	    pattern([P1, P2], Context);
	_ -> Context
    end.

exprs(Exprs, Context) ->
    exprs(Exprs, Context, none).

exprs(Exprs, Context, ReturnTypes) ->
    {Exprs1, Types1, Context1}  =exprs2(Exprs, Context, ReturnTypes),
    Exprs2 = lists:reverse(Exprs1),
    Context2 = 
	Context#context{trees = Context1#context.trees,
			func_types = Context1#context.func_types},
    case ReturnTypes of
	none -> {Exprs2, Context2};
	_ -> {Exprs2, Types1, Context2}
    end.

exprs2(Exprs, Context, none) ->
    lists:foldl(
      fun(Expr, {Exprs1, _Type, Context1}) ->
	      {Expr1, Context2} = expr(Expr, Context1),
	      {[Expr1 | Exprs1], undefined, Context2}
      end, {[], undefined, Context}, Exprs);
exprs2(Exprs, Context, all) ->
    lists:foldl(
      fun(Expr, {Exprs1, Types1, Context1}) ->
	      {Expr1, Type, Context2} = expr(Expr, Context1, true),
	      {[Expr1 | Exprs1], [Type | Types1], Context2}
      end, {[], [], Context}, Exprs);
exprs2(Exprs, Context, last) ->
    {Exprs3, Type3, Context3, _Count} =
	lists:foldl(
	  fun(Expr, {Exprs1, _Type1, Context1, 1}) ->
		  {Expr1, Type, Context2} = expr(Expr, Context1, true),
		  {[Expr1 | Exprs1], Type, Context2, 0};
	     (Expr, {Exprs1, _Type1, Context1, Count}) ->
		  {Expr1, Context2} = expr(Expr, Context1),
		  {[Expr1 | Exprs1], undefined, Context2, Count - 1}
	  end, {[], undefined, Context, length(Exprs)}, Exprs),
    {Exprs3, Type3, Context3}.

expr(Expr, Context) ->
    expr(Expr, Context, false).

expr(Expr, Context, ReturnType) ->
    expr(Expr, Context, ReturnType, false).

expr(Expr, Context, ReturnType, GetNestedFieldTypes) ->
    Res = case Expr of
	{record_field,L1,{var,L1,VarName} = Var,Field}  ->
	    case get_var_type(VarName, Context) of
		none ->
		    exit({type_inference_error, {VarName, L1}});
		{value, {record_type,RecName,FieldTypes}} ->
		    if GetNestedFieldTypes ->
			    {{record_field,L1,Var,RecName,Field}, Context,
			     FieldTypes};
		       true ->
			    {{record_field,L1,Var,RecName,Field}, Context}
		    end
	    end;
	
	%% nested inference -- first we infer the type of the
	%% outer variable, then we infer the type of its field (the
	%% inner variable), then we infer the type of the inner variable's
	%% field.
	{record_field,L1,{record_field,_,_,_} = Expr1,
	 {atom,_,RecName} = Name} ->
	    {Expr2, Context1, LastFields} =
		expr(Expr1, Context, false, true),
	    {record_field,_,_,_,{atom,_,OuterFieldName}} = Expr2,
	    case get_field_type(OuterFieldName, LastFields) of
		undefined ->
		    exit({type_inference_error, L1,
			  {OuterFieldName, RecName}});
		{record_type,RecName1,NextFields} ->
		    Result = {record_field, L1, Expr2, RecName1, Name},
		    if GetNestedFieldTypes ->
			    {Result, Context1, NextFields};
		       true ->
			    {Result, Context1}
		    end
	    end;
	%% single property setter, e.g. P.name = "bob"
 	{match,_L1,{record_field,_L2,_Expr,_FieldName} = RecField, Val} ->
 	    {FieldExpr, Context1} = expr(RecField, Context),
 	    {make_setter(FieldExpr, Val), Context1};
 	{match,L1,Expr1,Expr2} ->
	    {E1, Context1} = expr(Expr1, Context),
	    {E2, Context2} = expr(Expr2, Context1),
	    Context3 = match(E1, E2, Context2),
	    {{match, L1, E1, E2}, Context3};
 	{tuple,L,Elems} ->
				
	    {E1, Context1} = exprs(Elems, Context),
 	    {{tuple,L,E1}, Context1};
	{cons,L,Expr1,Expr2} ->
	    {E1, Context1} = expr(Expr1, Context),
	    {E2, Context2} = expr(Expr2, Context1),
	    {{cons,L,E1,E2}, Context2};
	{record,L,Name, Fields} ->
	    {F1, Context1} = exprs(Fields, Context),
	    {{record,L,Name,F1},Context1};
	{record,L,OtherVar,Name,Fields} ->
	    {F1, Context1} = exprs(Fields, Context),
	    {{record,L,OtherVar, Name,F1},Context1};
 	{op,L,Op,Expr1} ->
	    {E1, Context1} = expr(Expr1, Context),
	    {{op,L,Op,E1}, Context1};
 	{op,L,Op,Expr1,Expr2} ->
 	    {E1, Context1} = expr(Expr1, Context),
	    {E2, Context2} = expr(Expr2, Context1),
	    {{op,L,Op,E1,E2}, Context2};
	{'catch',L,Expr1} ->
	    {E1, Context1} = expr(Expr1, Context),
	    {{'catch',L,E1}, Context1};
	{call,L,{remote,L1,ModuleExpr,NameExpr},ParamExprs} ->
	    {M1, Context1} = expr(ModuleExpr, Context),
	    {N1, Context2} = expr(NameExpr, Context1),
	    {P1, Context3} = exprs(ParamExprs, Context2),
	    {{call,L,{remote,L1,M1,N1},P1}, Context3};
	{call,L, NameExpr, ParamExprs} ->
 	    {N1, Context1} = expr(NameExpr, Context),
 	    {P1, Context2} = exprs(ParamExprs, Context1),
	    Context3 =
		case NameExpr of
		    {atom,_,Name} ->
			case infer_func(Name, length(ParamExprs), Context) of
			    recursive ->
				Context2;
			    {_Form, Context4} ->
				Context4
			end;
		    _Other ->
			Context2
		end,
	    {{call,L,N1,P1}, Context3};

	%% todo infer the type returned from the LC expression
	{'lc',L,Result,Body} ->
	    {E2, _Context2} = exprs(Body, Context),
	    {{'lc',L,Result,E2}, Context};
	{'generate',L,Pattern,Expr1} ->
	    {E2, _Context2} = expr(Expr1, Context),
	    {{'generate',L,Pattern,E2}, Context};
%% 	{block, L, Exprs} ->
%% 	    {E1, Types, Context1} = exprs(Exprs, Context),
%% 	    {{block,L,lists:reverse(E1)}, Context1};
%% 	{'if', L, Clauses} ->
%% 	    {Clauses1, Type, Context1} = infer(Clauses, Context),
%% 	    {{'if', L, Clauses1}, Context1};
%% 	{'case', L, Clauses} ->
%% 	    {Clauses1, Type, Context1} = infer(Clauses, Context),
%% 	    {{'case', L, Clauses1}, Context1};
	Other ->
	    {Other, Context}
	  
	end,

    %% temporary hack
    if ReturnType ->
	    {Expr5, Context5} = Res,
	    {Type6, Context6} = get_type(Expr5, Context5),
	    {Expr5, Type6, Context6};
       true ->
	    Res
    end.

get_field_type(_FieldName, []) -> undefined;
get_field_type(FieldName, [{field_type,FieldName,Type}|_]) -> Type;
get_field_type(FieldName, [_|Rest]) -> get_field_type(FieldName, Rest).

make_var_expr([], _Line, Var) -> Var;
make_var_expr([{RecName, Field}], Line, Var) ->
    {record_field,Line,Var,RecName,Field};
make_var_expr([{RecName, Field} | Rest], Line, Var) ->
    Expr = make_var_expr(Rest, Line, Var),
    {record_field, Line, Expr, RecName, Field}.


make_setter(RecField, Val) ->
    make_setter(RecField, Val, []).
make_setter({record_field,Line,OuterField,RecName,Field}, Val,
	   NestedFields) ->
    {AllFields, Var} = get_setter_data
			 (OuterField,
			  [{RecName,Field} | NestedFields]),
    make_setter2(AllFields, [], Line, Var, Val).

get_setter_data({var,_,_VarName} = Var, NestedFields) -> {NestedFields, Var};
get_setter_data({record_field,_,{var,_,_VarName} = Var, RecName,
		 {atom,_,_FieldName} = Field}, NestedFields) ->
    {[{RecName, Field} | NestedFields], Var};
get_setter_data({record_field,_L,OuterField,RecName,Field}, NestedFields) ->
    get_setter_data(OuterField, [{RecName, Field} | NestedFields]).

make_setter2([{RecName, FieldName}], VisitedFields, Line,
		   Var, Val) ->
    {record,Line,make_var_expr(VisitedFields, Line, Var),RecName,
     [{record_field,Line,FieldName,Val}]};
make_setter2([{RecName, FieldName} = Field | Rest], VisitedFields, Line,
		   Var, Val) ->
    {record,Line,make_var_expr(VisitedFields, Line, Var),RecName,
     [{record_field,Line,FieldName,
       make_setter2(
	 Rest, [Field | VisitedFields], Line,
	 Var, Val)}]}.

match([], [], Context) -> Context;
match([Expr1|Rest1], [Expr2|Rest2], Context) when length(Rest1) ==
						length(Rest2) ->
    Context1 = match(Expr1, Expr2, Context),
    match(Rest1, Rest2, Context1);
match({var,_,Var}, {record,_,RecName,NewFields}, Context) ->
    %% this is a new record so we infer which fields are automatically
    %% instantiated
    case get_rec_type(RecName, Context) of
	none ->
	    exit({type_inference_error, RecName});
	{value, {record_type, _Name, _Fields} = Record} ->
	    add_field_types(Var, Record, NewFields, Context)
    end;
match({var,_,Var1}, {record,_,{var,L2,Var2},_RecName,NewFields}, Context) ->
    case get_var_type(Var2, Context) of
	none ->
	    exit({type_inference_error, {L2, Var2}});
	{value, {record_type,_Name,_FieldType} = Record} ->
	    add_field_types(Var1, Record, NewFields, Context)
    end;
match({var,_,Var}, Expr, Context) ->
    {ExprType, Context1} = get_type(Expr, Context),
    add_var_type(Var, ExprType, Context1);
match({cons,_,P1,P2}, {cons,_,P3,P4}, Context) ->
    Context1 = match(P1, P3, Context),
    match(P2, P4, Context1);
match({tuple,_,Elems1},{tuple,_,Elems2}, Context) ->
    match(Elems1, Elems2, Context);
match(_Expr1, _Expr2, Context) ->
    Context.

add_field_types(Var, Record, NewFields, Context) ->
    {NewRecType, Context1} =
	add_fields(Record, NewFields, Context),
    add_var_type(Var, NewRecType, Context1).

add_fields({record_type, RecName, ExistingFieldTypes}, NewFields, Context) ->
    PreservedFieldTypes =
		[FieldType || FieldType =
				  {field_type,
				   Name,_Val} <- ExistingFieldTypes,
			      not lists:member(
				    Name,
				    [NewName || {record_field,_,
						 {atom,_,NewName},
						 _Val1} <- NewFields])],
    {NewFieldTypes, Context1} = get_types(NewFields, Context),
    {{record_type,RecName,PreservedFieldTypes ++ NewFieldTypes}, Context1}.

get_types(Fields, Context) ->
    lists:foldl(
      fun(Field, {FieldTypes, Context1}) ->
	      {FieldType, Context2} = get_type(Field, Context1),
	      {[FieldType | FieldTypes], Context2}
      end, {[], Context}, lists:reverse(Fields)).

get_type({record,_,RecName,Fields}, Context) ->
    DefaultFieldTypes =
	case get_rec_type(RecName, Context) of
	    none ->
		exit(error2);
	    {value, {record_type, _RecName, FieldTypes4}} ->
		FieldTypes4
	end,
    {FieldTypes3, DefaultFieldTypes3, Context3} =
	lists:foldl(
	  fun(Field, {FieldTypes1, DefaultFieldTypes1, Context1}) ->
		  {{field_type, Name, _Val} = FieldType, Context2}
		      = get_type(Field, Context1),
		  DefaultFieldTypes2 =
		      lists:keydelete(Name, 2, DefaultFieldTypes1),
		  {[FieldType | FieldTypes1], DefaultFieldTypes2, Context2}
	  end, {[], DefaultFieldTypes, Context}, lists:reverse(Fields)),
    %% add the default types of the fields that have not been defined
    %% by the user
    {{record_type,RecName,FieldTypes3 ++ DefaultFieldTypes3}, Context3};
get_type({record,_,FieldExpr,_RecName,Fields}, Context) ->
    %% TODO why is RecName not used?
    {FieldType, Context1} = get_type(FieldExpr, Context),
    add_fields(FieldType, Fields, Context1);
get_type({record_field,_,{atom,_,Name}}, Context) ->
    {{field_type,Name,undefined}, Context};
get_type({record_field,_,{var,_,Var},_RecName,{atom,_,FieldName}}, Context) ->
    case get_var_type(Var, Context) of
	none ->
	    exit(bang);
	{value, {record_type,_RecName,FieldTypes}} ->
	    FieldType =
		lists:keysearch(FieldName, 2, FieldTypes),
	    {FieldType, Context}
    end;
get_type({record_field,_,{atom,_,Name},Val}, Context) ->
    Context1 = pattern(Val, Context),
    {ValType, Context2} = get_type(Val, Context1),
    {{field_type,Name,ValType}, Context2};
get_type({record_field,_,
	  {record_field,_,_Expr,_OuterRecName,_OuterFieldName} = OuterRec,
	  _InnerRecName,{atom,_,InnerFieldName}}, Context) ->
    {{record_type, _InnerRecName, FieldTypes}, Context1} = 
	get_type(OuterRec, Context),
    {get_field_type(InnerFieldName, FieldTypes), Context1};
get_type({var,_,VarName}, Context) ->
    case get_var_type(VarName, Context) of
	none ->
	    exit(error3);
	{value, Type} ->
	    {Type, Context}
    end;
get_type({cons,_,Head,{nil,_}}, Context) ->
    {HeadType, Context1} = get_type(Head, Context),
    {{list_type, single, [HeadType]}, Context1};
get_type({cons,_,HeadElt,Tail}, Context) ->
    {TailType, Context1} = get_type(Tail, Context),
    {HeadEltType, Context2} = get_type(HeadElt, Context1),
    NumTypes =
  	case TailType of
  	    {list_type, multi, TailEltType} -> multi;
  	    {list_type, single, [TailEltType]} when TailEltType == HeadEltType
  						  -> single;
  	    {list_type, single, TailEltType} -> multi
  	end,
    {{list_type, NumTypes, [HeadEltType | TailEltType]}, Context2};
get_type({tuple,_,Elems}, Context) ->
    {ElemTypes, Context2} =
	lists:mapfoldl(
	  fun get_type/2,
	  Context, lists:reverse(Elems)),
    {{tuple_type, ElemTypes}, Context2};

get_type({call,_,{atom,_,Name},ParamExprs}, Context) ->
    Arity = length(ParamExprs),
    case get_func_type(Name, Arity, Context) of
	none -> {undefined, Context};
	{value, {_Form, Type}} -> {Type, Context}
    end;

get_type(_Other, Context) -> {undefined, Context}.
