// Global because the parser uses it - urg TODO
function copy_type(x) {
  assert(x.node_type === "type");
  var res = new Object();
  res.node_type = "type";
  res.base_type = x.base_type;
  res.storage = x.storage;
  res.qualifiers = x.qualifiers;
  res.lvalue = x.lvalue;
  res.return_type = x.return_type;
  res.params = x.params;
  return res;
}

// This is lame and should be removed/changed
function assert(cond, msg) {
  if(!cond) {
    throw { message: "Assertion from function " + arguments.callee.caller.name + ": " + msg  };
  }
}

function JCC() {

  /*******************************/
  /* TYPE/SYMBOL TABLE UTILITIES */
  /*******************************/

  // Determines if two types are implicitly coercible
  function unifiable(x, y) {
    return x.base_type[0] === y.base_type[0]; // TODO
  }

  function pretty_print_type(t) {
    return t.base_type; // TODO: finish this!
  }

  function make_basic_type(t) {
    return { node_type: "type",
      base_type: [t],
      function_specifiers: [],
      qualifiers: [],
      storage: [],
      lvalue: false
    };
  }

  function alloc_size(x) { return 1; }

  function add_symbol(syms, def) {
    assert(typeof syms !== "undefined", "parent scope undefined");
    assert(typeof def.name !== "undefined", "def.vars.name undefined");
    assert(def.name !== "", "attempted to add an anonymous definition to the symbol table.");
    if(typeof syms[":" + def.name] === "undefined") { syms[":" + def.name] = def; }
    else { throw { message: "duplicated definition of " + def.name}; }
  }

  function find_symbol(node, name) {
    assert(typeof node !== "undefined", "asked to find a symbol in null");
    if(typeof node.symbols[":" + name] !== "undefined") {
      return node.symbols[":" + name];
    } else {
      if(node.parent == null) { // TODO: why does === not work?
        throw { message: "use of undeclared identifier " + name };
      } else {
        return find_symbol(node.parent, name);
      }
    }
  }

  /*********************/
  /* SEMANTIC ANALYSIS */
  /*********************/

  function analyze(node, acc) {
    assert(typeof acc !== "undefined", "acc was undefined");
    assert(typeof node !== "undefined", "node was undefined");
    var i;
    if(node === null) return;
    assert(typeof node.node_type !== "undefined", "only analyze node_type's");
    switch(node.node_type) {
      case "root":
        node.symbols = new Object();
        node.parent = (acc == null ? null : acc.parent_scope);
        acc = { parent_scope: node, iteration_type: "" };
        for(i = 0; i < node.globals.length; i++) {
          if(node.globals[i].node_type === "declaration") {
            if(node.globals[i].name === "") { throw { message: "anonymous global definition: dumb, right?"}; }
            add_symbol(node.symbols, node.globals[i]);
          } else {
            analyze(node.globals[i], acc);
          }
        }
        break;
      case "function_definition":
        add_symbol(acc.parent_scope.symbols, node);
        node.symbols = new Object();
        node.parent = acc.parent_scope;
        newacc = { parent_scope: node, expected_return: node.type.return_type };
        for(i = 0; i < node.type.params.length; i++) {
          if(node.type.params[i].node_type !== "param") {
            throw { message: "bad param for function" };
          }
          add_symbol(node.symbols, node.type.params[i]);
        }
        analyze(node.body, newacc);
        break;
      case "block":
        node.symbols = new Object();
        node.parent = acc.parent_scope;
        var oldparent = acc.parent_scope;
        acc.parent_scope = node;
        for(i = 0; i < node.contents.length; i++) {
          analyze(node.contents[i], acc);
          if(node.contents[i].node_type === "declaration") {
            add_symbol(node.symbols, node.contents[i]);
          }
        }
        acc.parent_scope = oldparent;
        break;
      case "if":
        analyze(node.cond, acc); // TODO: make sure bool-able
        analyze(node.then, acc);
        analyze(node.else, acc);
        break;
      case "for":
        analyze(node.init, acc);
        analyze(node.cond, acc); // TODO: make sure output is bool-able
        analyze(node.action, acc);

        var olditertype = acc.iteration_type;
        acc.iteration_type = "loop";
        analyze(node.body, acc);
        acc.iteration_type = olditertype;
        break;
      case "while":
        analyze(node.cond, acc); // TODO: make sure output is bool-able

        var olditertype = acc.iteration_type;
        acc.iteration_type = "loop";
        analyze(node.body, acc);
        acc.iteration_type = olditertype;
        break;
      case "switch":
        analyze(node.param, acc); // TODO: make sure that the type of thi is legit
        var olditertype = acc.iteration_type;
        acc.iteration_type = "switch";
        analyze(node.body, acc);
        acc.iteration_type = olditertype;
        break;
      case "declaration":
        if(node.value === null) break;
        analyze(node.value, acc);
        if(!unifiable(node.type, node.value.type)) {
          throw { message: "bad decl initializer" };
        }
        break;
      case "return":
        if(node.target === null) {
          if(!unifiable(acc.expected_return, make_basic_type("void"))) {
            throw { message: "returning void from non-void function."};
          }
        } else {
          analyze(node.target, acc);
          assert(typeof node.target.type !== "undefined", "went into return, got null type?");
          if(!unifiable(acc.expected_return, node.target.type)) {
            throw { message: "return: expected " + pretty_print_type(acc.expected_return)
              + " got " + pretty_print_type(node.target.type) };
          }
        }
        break;
      case "break":
        if(acc.iteration_type !== "loop" && acc.iteration_type !== "switch") {
          throw { message: "break statement not within a loop or switch." };
        }
        break;
      case "continue":
        if(acc.iteration_type !== "loop") {
          throw { message: "continue statement not within a loop." };
        }
        break;
      case "case":
        if(acc.iteration_type !== "switch") {
          throw { message: "case label statement not within a switch." };
        }
        analyze(node.guard, acc); // TODO: type check this (needs to reduce to integer constant)
        break;
      case "default":
        if(acc.iteration_type !== "switch") {
          throw { message: "default label statement not within a switch."};
        }
        break;
      case "expression":
        if(node.seqs.length == 0) {
          node.type = make_basic_type("void");
          break;
        }
        for(i = 0; i < node.seqs.length; i++) {
          analyze(node.seqs[i], acc);
        }
        node.type = copy_type(node.seqs[node.seqs.length - 1].type);
        break;
      case "function_call":
        analyze(node.func, acc);
        if(typeof node.func.type.return_type === "undefined") {
          throw { message: "non-function in function call position."};
        }
        if(node.func.type.params.length !== node.args.length) { // TODO: varargs
          throw { message: "number of arguments mismatch."};
        }
        for(i = 0; i < node.args.length; i++) {
          analyze(node.args[i], acc);
          if(!unifiable(node.args[i].type, node.func.type.params[i].type)) {
            throw { message: "type mismatch in function call."};
          }
        }
        node.type = copy_type(node.func.type.return_type);
        break;
      case "<": // TODO: boolean stuff
      case ">":
      case "==":
      case "<=":
      case ">=":
      case "!=":
      case "+":
      case "-":
      case "*":
      case "/":
        analyze(node.targets[0], acc);
        analyze(node.targets[1], acc);
        if(!unifiable(node.targets[0].type, node.targets[1].type)) {
          throw {message: "bad binop types."};
        }
        node.type = copy_type(node.targets[0].type);
        node.type.lvalue = false;
        break;
      case "post++":
      case "post--":
      case "pre--":
      case "pre++":
        analyze(node.target, acc);
        node.type = copy_type(node.target.type);
        if(node.type.lvalue !== true) {
          throw { message: "non-lvalue in increment/decrement."};
        }
        break;
      case "=":
        analyze(node.targets[0], acc);
        if(node.targets[0].type.lvalue !== true) {
          throw {message: "non-lvalue in assignment."};
        }
        analyze(node.targets[1], acc);
        if(!unifiable(node.targets[0].type, node.targets[1].type)) {
          throw {message: "assignment types mismatch."};
        }
        node.type = copy_type(node.targets[1].type);
        node.type.lvalue = false;
        break;
      case "unary*":
        throw {message: "sorry, no pointers yet!"};
        analyze(node.target, acc);
        // TODO: make sure node.type could be a pointer value
        // TODO: node.type should be made from node.target.type...
        node.type.lvalue = true;
        break;
      case "unary&":
        throw {message: "sorry, no pointers yet!"};
        analyze(node.target, acc);
        if(node.target.type.lvalue !== true) {
          throw { message: "expected lvalue"};
        }
        // TODO: construct node.type from node.target.type
        node.type.lvalue = false;
        break;
      case "constant":
        node.type = make_basic_type(node.value.node_type);
        break;
      case "identifier":
        node.type = copy_type(find_symbol(acc.parent_scope, node.expr).type);
        node.type.lvalue = true;
        break;
      default:
        throw {message: "analysis error: TODO: " + node.node_type };
    }
  }

  /************/
  /* CODE GEN */
  /************/

  function MakeGenSym(prefix) {
    var i = 0;
    var res = function() { return prefix + i++; };
    res.reset = function() { i = 0; };
    return res;
  }

  function MakeRegisterTable() {
    var regs = new Array(23);
    var res = new Object();
    var time = 0;
    offset = 6;
    res.get_reg = function(obj, asm) {
      var LRUslot = null;
      var mintime = 100;
      // First, look for an empty register.
      for(var i = 0; i < regs.length; i++) {
        if(regs[i] == null) {
          res.set_reg(i + offset, obj);
          return { reg: i + offset, restore: false };
        }
        else if(regs[i] != null && regs[i].t < mintime) {
          mintime = regs[i].t;
          LRUslot = i;
        }
      }

      // Ok, spill a temporary value to the stack.
      // TODO: vars that haven't been serialized to the stack yet
      asm.push(mips_push(LRUslot + offset));
      res.set_reg(LRUslot + offset, obj);
      return { reg: LRUslot + offset, restore: true };
    }
    res.check_regs = function(obj) {
      for(var i = 0; i < regs.length; i++) {
        if(regs[i] == null) continue;
        if(typeof regs[i] === 'string') continue;
        if(regs[i].d === obj) return i + offset;
      }
      return null;
    }
    res.set_reg = function(reg, obj) {
      regs[reg - offset] = { d: obj, t: time };
      time++;
    }
    res.invalidate_all = function() {
      for (var i = 0; i < regs.length; i++) {
        regs[i] = null;
      }
      time = 0;
    }
    res.invalidate = function(i) {
      if(i == 0) return;
      if(regs[i - offset] == null) return;
      if(typeof regs[i - offset].d !== 'string') return;
      regs[i - offset] = null;
    }
    res.reserve = function(i) {
      if(i == 0 || i > 5) throw { message: 'tried to reserve register ' + i + '.' };
      regs[i - 2]
    }
    res.invalidate_all();
    return res;
  }

  var while_gensym = MakeGenSym('while_');
  var endwhile_gensym = MakeGenSym('endwhile_');
  var else_gensym = MakeGenSym('else_');
  var endif_gensym = MakeGenSym('endif_');
  var for_gensym = MakeGenSym('for_');
  var endfor_gensym = MakeGenSym('endfor_');

  var MIPSreg =
    { SP: 29,
      FP: 30,
      LR: 31 };

  function gen(node) {
    while_gensym.reset();
    endwhile_gensym.reset();
    else_gensym.reset();
    endif_gensym.reset();
    for_gensym.reset();
    endfor_gensym.reset();
    var asm = new Array();
    var acc = new Object();
    acc.regs = MakeRegisterTable();
    acc.scope = null;
    for(var i = 0; i < node.globals.length; i++) {
      gen_statement(node.globals[i], asm, acc);
    }
    return asm;
  }


  function gen_statement(node, asm, acc) {
    var i;
    switch(node.node_type) {
      case 'block':
        if(node.contents == null) break;
        var len = 0;
        if(typeof node.symbols === 'undefined') throw {message: 'what'};
        for(sym in node.symbols) {
          if(node.symbols.hasOwnProperty(sym)) {
            len += alloc_size(node.symbols[sym].type);
            node.symbols[sym].loc = acc.fp_offset + len;
          }
        }
        if(len + acc.alloci_debt != 0) {
          asm.push(mips_allocai(len + acc.alloci_debt));
          acc.fp_offset += acc.alloci_debt;
          acc.alloci_debt = 0;
        }
        acc.fp_offset += len;
        var oldscope = acc.scope;
        acc.scope = node;
        for(i = 0; i < node.contents.length; i++) {
          if(typeof node.contents[i].node_type !== 'undefined' && node.contents[i].node_type === 'decl') {
            assert(0 == 1); // TODO what?
          }
          gen_statement(node.contents[i], asm, acc);
        }
        acc.scope = oldscope;
        break;
      case 'expression':
        var src1 = gen_expr(node, asm, acc);
        break;
      case 'return':
        var src1 = 0;
        if(node.target !== null) {
          var src1 = gen_expr(node.target, asm, acc);
        }
        asm.push(mips_ret(src1));
        acc.regs.invalidate(src1);
        break;
      case 'function_definition':
        asm.push('_' + node.name + ':');
        acc.fp_offset = 0;
        var len = 0;
        for(i = node.type.params.length - 1; i >= 4; i--) {
          len += alloc_size(node.type.params[i].type);
          node.symbols[':'+ node.type.params[i].name].loc = -len - 25*4;
        }
        acc.alloci_debt = 0;
        for(i = 0; i < Math.min(4, node.type.params.length); i++) {
          var sym = find_symbol(node, node.type.params[i].name);
          var dst = acc.regs.get_reg(sym);
          sym.loc = acc.alloci_debt;
          acc.alloci_debt += alloc_size(node.type.params[i].type);
          asm.push(mips_add(dst.reg, 0, i + 2));
        }

        gen_statement(node.body, asm, acc);
        // TODO: should output a return-canary here...
        asm.push('');
        acc.regs.invalidate_all();
        break;
      case 'if':
        var src1 = gen_expr(node.cond, asm, acc);
        acc.regs.invalidate(src1);
        if(node.else !== null) else_lbl = else_gensym();
        endif_lbl = endif_gensym();
        if(node.else !== null) asm.push(mips_beq(0, src1, else_lbl));
        else                   asm.push(mips_beq(0, src1, endif_lbl));
        gen_statement(node.then, asm, acc);
        if(node.else !== null) {
          asm.push(else_lbl + ': ');
          gen_statement(node.else, asm, acc);
        } else {
          asm.push(endif_lbl + ': ');
        }
        break;
      case 'for':
        for_lbl = for_gensym();
        endfor_lbl = endfor_gensym();
        var oldloopstart = acc.loop_start;
        var oldloopexit = acc.loop_exit;
        acc.loop_start = for_lbl;
        acc.loop_exit = endfor_lbl;
        var src1 = gen_expr(node.init, asm, acc);
        acc.regs.invalidate(src1);
        asm.push(for_lbl + ': ');
        var src1 = gen_expr(node.cond, asm, acc);
        acc.regs.invalidate(src1);
        asm.push(mips_beq(src1, 0, endfor_lbl));
        if(node.action !== null) {
          var src1 = gen_expr(node.action, asm, acc);
          acc.regs.invalidate(src1);
        }
        gen_statement(node.body, asm, acc);
        asm.push(mips_j(for_lbl));
        asm.push(endfor_lbl + ': ');
        acc.loop_start = oldloopstart;
        acc.loop_exit = oldloopexit;
        break;
      case 'while':
        while_lbl = while_gensym();
        endwhile_lbl = endwhile_gensym();
        var oldloopstart = acc.loop_start;
        var oldloopexit = acc.loop_exit;
        acc.loop_start = while_lbl;
        acc.loop_exit = endwhile_lbl;
        asm.push(while_lbl + ': ');
        var src1 = gen_expr(node.cond, asm, acc);
        acc.regs.invalidate(src1);
        asm.push(mips_beq(src1, 0, endwhile_lbl));
        gen_statement(node.body, asm, acc);
        asm.push(mips_j(while_lbl));
        asm.push(endwhile_lbl + ': ');
        acc.loop_exit = oldloopexit;
        acc.loop_start = oldloopstart;
        break;
      case 'break':
        asm.push(mips_j(acc.loop_exit));
        break;
      case 'continue':
        asm.push(mips_j(acc.loop_start));
        break;
      case 'declaration':
        if(node.value !== null) {
          var dst = find_symbol(acc.scope, node.name).loc;
          var src = gen_expr(node.value, asm, acc);
          asm.push(mips_sw(src, MIPSreg.FP, 4*dst));
          acc.regs.invalidate_all();
        }
        break;
      default:
        throw { message: 'could not gen for: ' + node.node_type };
    }
  }

  function gen_lvalue(node, asm, acc) {
    switch(node.node_type) {
      case 'identifier':
        var sym = find_symbol(acc.scope, node.expr);
        return { reg: MIPSreg.FP, offset: sym.loc };
      default:
        throw { message: "invalid lvalue (or not implemented." };
    }
  }

  function gen_expr(node, asm, acc) {
    var i;
    var dst = null;
    if(typeof acc.dst !== 'undefined') {
      dst = acc.dst;
      delete acc.dst;
    }
    switch(node.node_type) {
      case 'expression':
        for(i = 0; i < node.seqs.length - 1; i++) {
          acc.dst = 0;
          var src1 = gen_expr(node.seqs[i], asm, acc);
          acc.regs.invalidate(src1);
        }
        acc.dst = dst;
        var dst = gen_expr(node.seqs[i], asm, acc);
        return dst;
      case 'function_call':
        for(var i = 0; i < node.args.length; i++) {
          if(i < 4) { acc.dst = i + 2; }
          var src1 = gen_expr(node.args[i], asm, acc);
          if(i >= 4) {
            asm.push(mips_push(src1));
            acc.regs.invalidate(src1);
          }
        }
        asm.push(mips_call("_" + node.func.expr)); // TODO you wish!
        for(var i = 0; i < Math.min(4, node.args.length); i++) { acc.regs.invalidate(i + 2); }
        if(node.args.length > 4) asm.push(mips_popn(node.args.length - 4));
        if(dst === 0) return;
        if(dst === null) var dst = acc.regs.get_reg('temp', asm).reg;
        if(dst !== 1) asm.push(mips_add(dst,0,1));
        return dst;
      case "=":
        var dst = gen_lvalue(node.targets[0], asm, acc);
        var src = gen_expr(node.targets[1], asm, acc);
        asm.push(mips_sw(src, dst.reg, 4*dst.offset));
        acc.regs.invalidate_all();
        // NOTE: see "declaration" in gen_statement. duplicates this code :(
        break;
      case '<':
      case '+':
      case '-':
      case '*':
      case '/':
        function gen_binop(op, dst, src1, src2) {
          switch(op) {
            case '<': return mips_slt(dst, src1, src2);
            case '+': return mips_add(dst, src1, src2);
            case '-': return mips_sub(dst, src1, src2);
            case '*': return mips_mult(dst, src1, src2);
            default: throw { message: "opps" };
          }
        }
        var src1 = gen_expr(node.targets[0], asm, acc);
        var src2 = gen_expr(node.targets[1], asm, acc);
        acc.regs.invalidate(src1);
        acc.regs.invalidate(src2);
        if(dst === 0) return 0;
        if(dst === null) dst = acc.regs.get_reg('temp', asm).reg;
        asm.push(gen_binop(node.node_type, dst, src1, src2));
        return dst;
      case 'constant':
        if(dst === 0) return 0;
        if(dst === null) {
          if(node.value.value == 0) return 0;
          dst = acc.regs.get_reg('temp', asm).reg;
        }
        asm.push(mips_addi(dst, 0, node.value.value));
        return dst;
      case 'identifier':
        if(dst === 0) return 0;
        var sym = find_symbol(acc.scope, node.expr);
        var src1 = acc.regs.check_regs(sym);
        if(src1 != null) {
          if(dst != null && dst !== src1) {
            asm.push(mips_add(dst,0,src1));
            return dst;
          }
          return src1;
        }
        if(dst == null) dst = acc.regs.get_reg(sym, asm);
        asm.push(mips_lw(dst.reg, MIPSreg.FP, 4*sym.loc));
        return dst.reg;
      default:
        throw { message: 'could not gen for: ' + node.node_type };
    }
  }

  /*****************/
  /* MIPS EMITTERS */
  /*****************/

  function mips_push(src) { return 'push $' + src; }
  function mips_pop(src)  { return 'pop $' + src; }
  function mips_add(dst, src1, src2)  { return 'add $' + dst + ', $' + src1 + ', $' + src2; }
  function mips_addi(dst, src1, src2) { return 'addi $' + dst + ', $' + src1 + ', ' + src2; }
  function mips_sub(dst, src1, src2)  { return 'sub $' + dst + ', $' + src1 + ', $' + src2; }
  function mips_mult(dst, src1, src2) { return 'mult $' + src1 + ', $' + src2 + '\nmflo $' + dst; }
  function mips_slt(dst, src1, src2)  { return 'slt $' + dst + ', $' + src1 + ', $' + src2; }
  function mips_mflo(dst) { return 'mflo $' + dst; }
  function mips_mfhi(dst) { return 'mfhi $' + dst; }
  function mips_bne(src1, src2, loc) { return 'bne $' + src1 + ', $' + src2 + ', ' + loc; }
  function mips_beq(src1, src2, loc) { return 'beq $' + src1 + ', $' + src2 + ', ' + loc; }
  function mips_j(loc) { return 'j ' + loc; }
  function mips_jr(src) { return 'jr $' + src; }
  function mips_lw(dst, src, off) { return 'lw $' + dst + ', ' + off + '($' + src + ')'; }
  function mips_sw(dst, src, off) { return 'sw $' + dst + ', ' + off + '($' + src + ')'; }
  function mips_allocai(imm) { return 'allocai ' + imm; }
  function mips_call(dst) { return 'call ' + dst; }
  function mips_ret(src)     { return 'ret $' + src; }

  /***************/
  /* PUBLIC DATA */
  /***************/

  var res = new Object();
  res.compile = function(code) {
    if(typeof res.parse_tree !== 'undefined') delete res.parse_tree;
    if(typeof res.asm !== 'undefined') delete res.asm;
    res.parse_tree = c.parse(code);
    if(res.parse_tree.node_type === 'expression_shortcut') {
      throw { message: 'expression in global scope - not good.' };
    }
    analyze(res.parse_tree, null);
    res.asm = gen(res.parse_tree).join('\n');
  }

  res.repl_compile = function(code) {
    if(typeof res.asm === 'undefined') throw { message: "fix definitions before using repl." };
    var repl_pt = c.parse(code);
    if(repl_pt.node_type !== 'expression_shortcut') {
      throw { message: 'bad repl input' };
    }
    var repl_pt_dummy = c.parse('int foo() { return; }');
    repl_pt_dummy.globals[0].body.contents[0].target = repl_pt.expr;
    analyze(repl_pt_dummy, { parent_scope: res.parse_tree });
    repl_asm = "entry:\ncall repl\nhalt\nrepl:\n" + gen(repl_pt_dummy).slice(1).join('\n');
    return repl_asm;
  }

  return res;
}
