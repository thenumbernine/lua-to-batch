#!/usr/bin/env lua

local parser = require 'parser'
local ast = require 'parser.lua.ast'
local assert = require 'ext.assert'
local table = require 'ext.table'
local path = require 'ext.path'
local range = require 'ext.range'

local infile = ...

-- [=[ copied out of parser/tests/lua_to_c.lua , in case you want to put it all in one place ...
local tabs = -1	-- because everything is in one block
function tab()
	return ('\t'):rep(tabs)
end
function tabblock(t, consume)
	tabs = tabs + 1
	for i,ti in ipairs(t) do
		consume(tab())
		consume(ti)
		if i < #t then consume'\n' end
	end
	tabs = tabs - 1
end

-- TODO subclass into a separate ast
for k,cl in pairs(ast) do
	if ast.node:isa(cl) then
		function cl:toBatch()
			--[[ this does a lot of lua-specific spacing stuff
			return cl:serializeRecursiveMember'toBatch_recursive'
			--]]
			-- [[
			local s = ''
			local consume
			consume = function(x)
				if type(x) == 'number' then
					x = tostring(x)
				end
				if type(x) == 'string' then
					s = s .. x
				elseif type(x) == 'table' then
					assert.is(x, ast.node)
					assert.index(x, 'toBatch_recursive')
					x:toBatch_recursive(consume)
				else
					error('here with unknown type '..type(x))
				end
			end
			self:toBatch_recursive(consume)
			return s
			--]]
		end
		-- weakness to this design ...i need to always keep specifying the above toC() wrapper, or I have to make a seprate member function...
		function cl:toBatch_recursive(consume)
			self:serialize(consume)
		end
	end
end
-- ]=]


local varindex = 0
local function nextvar()
	varindex = varindex + 1
	return '__tmpvar__'..varindex
end



local gotoindex = 0
local function nextgoto()
	gotoindex = gotoindex + 1
	return '__tmpgoto__'..gotoindex
end

local funcindex = 0
local function nextfunc()
	funcindex = funcindex + 1
	return '__tmpfunc__'..funcindex
end


function ast._concat:toBatch_recursive(consume)
	consume(self[1])
	consume(self[2])
end

function ast._call:toBatch_recursive(consume)
	local funcname
	if ast._var:isa(self.func) then
		funcname = self.func.name
		if funcname == 'select' then
			error("select() should have be replaced already")
		end
		if funcname == 'print' then
			consume'echo '
			for i,x in ipairs(self.args) do
				consume(x)
				if i < #self.args then
					consume'\t'
				end
			end
			return
		end
	elseif ast._index:isa(self.func) then
		if ast._var:isa(self.func.expr)
		and self.func.expr.name == 'os'
		and ast._string:isa(self.func.key)
		and self.func.key.value == 'exit'
		then
			-- os.exit(1) ... returns an error ... but shouldn't kill calling batch files, right?
			consume'goto :eof'
			return
		end
	end
	consume'call :'
	consume(self.func)
	for _,arg in ipairs(self.args) do
		consume' '
		consume(arg)
	end
end

function ast._string:toBatch_recursive(consume)
	consume(self.value)	-- no quotes
end

function ast._local:toBatch_recursive(consume)
	-- local has function or assign as children
	-- if an assign isn't the child of a local then it will need to be exported at the end of a setlocal block (endlocal & set ... )
	-- otherwise, upon local, we will need a setlocal block
	-- if we're inside a function ... ever ... then when the first setlocal is executed, it must be followed with EnableDelayedExpansion
	--  and then all subsequent local variable references will need to be surrounded by !'s instead of %'s
	consume(self.exprs[1])
end

function ast._foreq:toBatch_recursive(consume)
	-- if a for-loop arg is an expression, can it be evaluated immediately?
	-- for-loop vars must have two parenthesis prefix
	consume'for /l %%'
	consume(self.var.name)
	consume' in ('
	consume(self.min)
	consume','
	if self.step then
		consume(self.step)
	else
		consume'1'
	end
	consume','
	consume(self.max)
	consume') do (\n'
	-- TODO call into loop body, and exit /b
	tabblock(self, consume)
	consume'\n)'
end

function ast._if:toBatch_recursive(consume)
	consume'if '
	consume(self.cond)
	consume' (\n'
	tabblock(self, consume)
	for _,ei in ipairs(self.elseifs) do
		consume(ei)
	end
	if self.elsestmt then 
		consume(self.elsestmt) 
	end
	consume'\n'
	consume(tab())
	consume')'
end
function ast._elseif:toBatch_recursive(consume)
	consume'\n'
	consume(tab())
	consume') else if '
	consume(self.cond)
	consume' (\n'
	tabblock(self, consume)
end

for _,info in ipairs{
	{'lt','lss'},
	{'le','leq'},
	{'gt','gtr'},
	{'ge','geq'},
	{'eq','equ'},
	{'ne','neq'},
} do
	local name, sym = table.unpack(info)
	ast['_'..name].toBatch_recursive = function(self, consume)
		for i,x in ipairs(self) do
			consume(x)
			if i < #self then
				consume' '
				consume(sym)
				consume' '
			end
		end
	end
end

function ast._block:toBatch_recursive(consume)
	tabblock(self, consume)
end
--[[
function ast._vararg:toBatch_recursive(consume)
if the ... is in global scope then it will work as an accessor to $*
if it's in function scope then ... the same?
in both cases, ... isn't converted directly, but instead, how it's used
end
--]]

local varmap = {}
local function getBatchVarForLuaVar(varname)
	local batchvar = varmap[varname]
	if not batchvar then
		batchvar = varname	-- TODO sanitize
		varmap[varname] = batchvar
	end
	return batchvar
end

function ast._assign:toBatch_recursive(consume)
	for i=1,#self.vars do
		local var = getBatchVarForLuaVar(self.vars[i])
		local expr = self.exprs[i]
		consume'set '
		if self.arith then
			consume'/a '
		end
		consume('"'..var.name..'=')
		consume(expr)
		consume'"'
		if i < #self.vars then
			consume'\n'
		end
	end
end

local function findsource(src)
	local node = src
	while node do
		if ast._foreq:isa(node) and node.var.name == src.name then return node end
		node = node.parent
	end
end

function ast._var:toBatch_recursive(consume)
	local name = self.name
	local varsource = findsource(self)
	if varsource and ast._foreq:isa(varsource) then
		name = '%%' .. name
	else
		if type(name) == 'number' then
			name = '%' .. name
		else
			if not ast._index:isa(self.parent) then
				name = '!'..name..'!'
			end
		end
	end
	consume(name)
end

function ast._index:toBatch_recursive(consume)
	consume'!'
	consume(self)
	consume'!'
end

local _argcount = ast.nodeclass'argcount'
function ast._argcount:init(...)
end
function ast._argcount:toBatch_recursive(consume)
	consume[[
set __tmp__argcount=0
for %%x in (%*) do (
	set /a __tmp__argcount+=1
	set "__tmp__argvalue[!__tmp__argcount!]=%%x"
)]]
end

-- modulus is two %'s
-- along with the two % prefix to for-loops and % wrappers to variables, this is not confusing at all
function ast._mod:toBatch_recursive(consume)
	for i,x in ipairs(self) do
		consume(x)
		if i < #self then
			consume' %% '
		end
	end
end

function ast._function:toBatch_recursive(consume)
	-- if it is a local function then move it to global scope ... this means closures are in danger of being invalid
	-- args will have to be remapped beforehand as well...
	local name = assert(self.name)	-- if it's a lambda then generate it a name
	assert.is(name, ast._var)
	name = name.name	-- TODO function .name is a var, with .name a string (should it be a _string ?)
	local l = nextgoto()
	-- for safety's sake I'll add gotos around the function 
	-- so global scope code keeps executing 
	consume'\n'
	consume('goto '..l..'\n')
	consume(':'..name..'\n')
	tabblock(self, consume)
	consume'\n'
	consume'exit /b\n'
	consume(':'..l)
end

function ast._goto:toBatch_recursive(consume)
	consume('goto ')
	consume(self.name)
end

ast._label = ast.nodeclass'label'
function ast._label:init(name)
	self.name = name
end
function ast._label:toBatch_recursive(consume)
	consume'\n:'
	consume(self.name)
end

-- notice this doesn't return any values
-- I'm only using this for inserting an exit at the end of the global scope block, before all temp functions
--  I should also move all other funtcions beneath this inserted statement...
function ast._return:toBatch_recursive(consume)
	consume'exit /b'
end

function ast._or:toBatch_recursive(consume)
	error("should've replaced all of the or's")
end
function ast._and:toBatch_recursive(consume)
	error("should've replaced all of the and's")
end

ast._endlocal = ast.nodeclass'endlocal'
function ast._endlocal:init(globals)
	self.globals = table(globals)
end
function ast._endlocal:toBatch_recursive(consume)
	-- TODO & set VARIABLE=value at the end
	-- for all the global variables we assigned to 
	local t = tab()
	consume('\n'..t..'endlocal')
	t = t .. '\t'
	for _,name in ipairs(self.globals) do
		consume(' ^\n'
			..t..'& set "'..name..'=%'..name..'%"')
	end
end

infile = infile or 'test1.lua'
local outfile = path(infile):setext'bat'
print('reading '..infile)
local code = path(infile):read()
local tree = parser.parse(code)

-- if there is a _call to "select('#', ...)" then find the parent block and put some argcount code at the beginning
local found
tree:traverse(function(node)
	if ast._call:isa(node)
	and node.func.name == 'select'
	and ast._vararg:isa(node.args[2])
	then
		if ast._string:isa(node.args[1])
		and node.args[1].value == '#'
		then
			local _, p = node:ancestors():find(nil, function(x) return ast._block:isa(x) end)
			assert(p, "expected a parent to be a block")
			found = p
			return ast._var'__tmp__argcount'
		end
		return ast._index(
			ast._var'__tmp__argvalue',
			node.args[1]
		)
	end
	return node
end)
-- don't insert while traversing or else...
--if found then
table.insert(tree, 1, ast._argcount())
--end
ast.refreshparents(tree)


-- break down all composite expressions into temp stmts
local assignsForStmts = {}
local function opis(node)
	return (ast._op:isa(node) or ast._par:isa(node))
	and not (
	-- not handling boolean right now
		ast._eq:isa(node)
		or ast._ne:isa(node)
		or ast._ge:isa(node)
		or ast._gt:isa(node)
		or ast._le:isa(node)
		or ast._lt:isa(node)
		or ast._and:isa(node)
		or ast._or:isa(node)
		or ast._not:isa(node)
	)
end
tree:traverse(nil, function(node)
	if opis(node) then
		--  then move node to before the containing statement 
		local _, p = node:ancestors():find(nil, function(x) return ast._stmt:isa(x) end)
		if ast._local:isa(p.parent) then p = p.parent end

		local function replace(arg)
			local varname = nextvar()
			local var = ast._var(varname)
			local assign = ast._local{ast._assign({var}, {arg})}
			assignsForStmts[p] = assignsForStmts[p] or table()
			assignsForStmts[p]:insert(assign)
			return var
		end

		if ast._op:isa(node) then
			for i=1,#node do
				if opis(node[i]) then
					node[i] = replace(node[i])
				end
			end
		elseif ast._par:isa(node) then
			--node.expr = replace(node.expr)
			node = node.expr
		end
	end
	return node
end)
tree:traverse(function(node)
	local assigns = assignsForStmts[node]
	if assigns then
		assigns:insert(node)
		return ast._block(assigns:unpack())
	end
	return node
end)

-- now replace 


-- if there is an assignment, then check the statement for operations
-- if it has string literals then use default assignment
-- if it has arithmetic of any sort then use /a on the assignment's "set"
-- TODO in fact, we might have to break down all expressions into their own statements
tree:traverse(function(node)
	if ast._add:isa(node)
	or ast._sub:isa(node)
	or ast._mul:isa(node)
	or ast._div:isa(node)
	or ast._mod:isa(node)
	then
		local _, p = node:ancestors():find(nil, function(x) return ast._stmt:isa(x) end)
		assert(p, "expected a parent to be a stmt")
		p.arith = true
	end
	return node
end)


-- TODO
-- move all functions to global namespace


-- rename all function arguments to %'s
tree:traverse(function(node)
	if ast._function:isa(node) then
		-- TODO make sure the function is global scope
		local args = table.mapi(node.args, function(arg) 
			assert.type(arg.name, 'string')
			return arg.name 
		end)
		return node:traverse(function(node2)
			if ast._var:isa(node2) then
				assert.type(node2.name, 'string')
				local i = table.find(args, node2.name)
				if i then
					return ast._var(i)
				end
			end
			return node2
		end)
	end
	return node
end)


-- replace foreq bodies with function calls
-- this messes up scope of variables.
-- TODO make sure everything is passed into the function
--  then rename all variable references accordingly
-- this is bad because if and or exprs need to be unraveled as gotos in batch,
--  and goto inside of for-loops fail (unless they are calls)
local newfuncs = table()
tree:traverse(function(node)
	if ast._foreq:isa(node) then
		local newfuncname = nextfunc()
		local stmts = {table.unpack(node)}
		-- replace all instances of the for-loop variable with the function arg %1
		for _,stmt in ipairs(stmts) do
			stmt:traverse(function(node2)
				if ast._var:isa(node2) then
					if node2.name == node.var.name then
						return ast._var(1)
					end
				end
				return node2
			end)
		end
		newfuncs:insert(ast._function(
			ast._var(newfuncname),
			{ast._var(1)},
			table.unpack(stmts)
		))
		-- any harm in removing nodes? or better to return new objects?
		for i=#node,1,-1 do node[i] = nil end
		node[1] = ast._call(
			ast._var(newfuncname),
			node.var
		)
	end
	return node
end)
for _,f in ipairs(newfuncs) do
	table.insert(tree, f)
end
ast.refreshparents(tree)


--[[
if is tricky...
since batch supports no boolean operators in if conditions,
 and batch does support gotos
 I'll treat the transpiler like an asm code generator:
if there's an 'if' stmt then generate 2 labels
	and do the short-circuit evaluation
	replacing all 'a and b's with 'not (not a or not b)'
--]]
tree:traverse(function(node)
	local function handle_if(
		cond,
		ifstmts,
		elseifs,
		elsestmt
	)
		local l1 = nextgoto()
		local l2 = nextgoto()
		
		local stmts = table()

		local function processcond(boolexpr, l1, l2, nott)
			if ast._or:isa(boolexpr) then
				processcond(boolexpr[1], l1, l2, nott)
				processcond(boolexpr[2], l1, l2, nott)
			elseif ast._and:isa(boolexpr) then
				processcond(boolexpr[1], l2, l1, not nott)
				processcond(boolexpr[2], l2, l1, not nott)
			elseif ast._not:isa(boolexpr) then
				processcond(boolexpr[1], l1, l2, not nott)
			else
				if nott then boolexpr = ast._not(boolexpr) end
				stmts:insert(ast._if(boolexpr, ast._goto(l1)))
			end
		end
		processcond(cond, l1, l2, false)
		-- if condition is false:
		-- (else stmts goes here)
		-- TODO handle elseifs like nested if's in an else
		if #elseifs > 0 or elsestmt then
			-- handle else alone
			if #elseifs == 0 then
				for _,n in ipairs(elsestmt) do
					stmts:insert(n)
				end
			else
			-- handle else and then if's
				elseifs = table(elseifs)
				local firstelseif = elseifs:remove(1)
				stmts:insert(handle_if(
					firstelseif.cond,
					firstelseif,
					elseifs,
					elsestmt
				))
			end
		end
		stmts:insert(ast._goto(l2))
		-- if condition is true:
		stmts:insert(ast._label(l1))
		for _,n in ipairs(ifstmts) do
			stmts:insert(n)
		end
		stmts:insert(ast._label(l2))
		return ast._block(stmts:unpack())
	end
	
	if ast._if:isa(node) then
		return handle_if(
			node.cond,		-- cond
			node,			-- stmts
			node.elseifs,	-- elseifs
			node.elsestmt	-- else
		)
	end
	return node
end)

-- keep track of all global assignments
-- notice I'm not keeping track of scope
local locals = {}
local globals = {}
tree:traverse(function(node)
	if ast._assign:isa(node) then
		local names = table.mapi(node.vars, function(var)
			assert.type(var.name, 'string')
			return var.name
		end)
		if ast._local:isa(node.parent) then
			for _,name in ipairs(names) do
				locals[name] = true
			end
		else
			for _,name in ipairs(names) do
				globals[name] = true
			end
		end
	end
	return node
end)
for name,_ in ipairs(globals) do
	if locals[name] then
		print("Warning, you used the variable '..name..' as a local and a global.  I'm not keeping track of scope.")
	end
end

table.insert(tree, ast._endlocal(table.keys(globals)))

outfile:write(
	table{
		'@echo off',
		'setlocal enabledelayedexpansion',
		tree:toBatch()
	}:concat'\n'
		-- until I can solve my tab problems:
		:gsub('\t+', '\t')
)
print('writing '..outfile)
