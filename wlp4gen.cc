#include <string>
#include <iostream>
#include <set>
#include <sstream>
#include <utility>
#include <vector>
#include <map>
using namespace std;

class Tree{
public:
	string rule;
	vector<string> tokens;
	vector<Tree> children;
};

// global symbol table
map<string, pair<vector<string>,map<string, pair<string, int> > > >tables;
string currentActive;
int whileCount = 0;
int ifCount = 0;
int skipCount = 0;

string typeOf(const Tree& parseTree);
void generateCode(const Tree& parseTree);
	
string paramCheck(Tree parseTree, string beingCalled, int currSig){
	// end of arglist
	if (parseTree.rule == "arglist expr"){
		/*cerr << "current: " << currentActive << " rule: " << parseTree.children[0].rule << endl;
		cerr << "type of param " << typeOf(parseTree.children[0]) << endl;
		cerr << "type of param needed " << tables[currentActive].first[currSig] << endl;*/
		int size = tables[beingCalled].first.size();
		if (currSig != size - 1){
			cerr << "ERROR: number of params given to " << beingCalled << " incorrect" << endl;
			throw(out_of_range("i"));
		} else if (tables[beingCalled].first[currSig] != typeOf(parseTree.children[0])){
			cerr << "ERROR: " << currSig << "th parameter of " << beingCalled << " does not match" << endl;
			throw(out_of_range("i"));
		} else {
			return typeOf(parseTree.children[0]);
		}
	}

	// middle of arglist
	else if (parseTree.rule == "arglist expr COMMA arglist"){
		int size = tables[beingCalled].first.size();
		if (currSig < size){
			if (tables[beingCalled].first[currSig] != typeOf(parseTree.children[0])){
				cerr << "ERROR: " << currSig << "th parameter of " << beingCalled << " does not match" << endl;
				throw(out_of_range("i"));
			} else {
				return paramCheck(parseTree.children[2], beingCalled, currSig+1);
			}
		} else {
			cerr << "ERROR: too much params given" << endl;
			throw(out_of_range("i"));
		}
	}

	else {return "";}
}

string typeOf(const Tree& parseTree){

	// update current procedure
	if (parseTree.rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		currentActive = parseTree.children[1].tokens[1];
	} else if (parseTree.rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		currentActive = parseTree.children[1].tokens[1];
	}

	// direct
	if (parseTree.rule == "expr term" or parseTree.rule == "term factor" or parseTree.rule == "factor ID" or parseTree.rule == "lvalue ID"){
		return typeOf(parseTree.children[0]);
	} else if (parseTree.rule == "factor NUM" or parseTree.rule == "type INT"){
		return "int";
	} else if (parseTree.rule == "factor NULL" or parseTree.rule == "type INT STAR"){
		return "int*";
	} else if(parseTree.tokens[0] == "ID"){
		return tables[currentActive].second[parseTree.tokens[1]].first;
	} else if(parseTree.tokens[0] == "NUM"){
		return "int";
	}

	// addtion
	else if (parseTree.rule == "expr expr PLUS term"){
		if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int"){
			return "int";
		} else if (typeOf(parseTree.children[0]) == "int*" and typeOf(parseTree.children[2]) == "int"){
			return "int*";
		} else if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int*"){
			return "int*";
		} else {
			cerr << "ERROR: invalid addition type" << endl;
			throw(out_of_range("i"));
		}
	}

	// subtraction
	else if (parseTree.rule == "expr expr MINUS term"){
		if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int"){
			return "int";
		} else if (typeOf(parseTree.children[0]) == "int*" and typeOf(parseTree.children[2]) == "int"){
			return "int*";
		} else if (typeOf(parseTree.children[0]) == "int*" and typeOf(parseTree.children[2]) == "int*"){
			return "int";
		} else {
			cerr << "ERROR: invalid subtraction type" << endl;
			throw(out_of_range("i"));
		}
	}

	// multiplication
	else if (parseTree.rule == "term term STAR factor"){
		if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int"){
			return "int";
		} else {
			cerr << "ERROR: invalid multiplication type" << endl;
			throw(out_of_range("i"));
		}
	}

	// division
	else if (parseTree.rule == "term term SLASH factor"){
		if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int"){
			return "int";
		} else {
			cerr << "ERROR: invalid division type" << endl;
			throw(out_of_range("i"));
		}
	}

	// mudulo
	else if (parseTree.rule == "term term PCT factor"){
		if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int"){
			return "int";
		} else {
			cerr << "ERROR: invalid modulo type" << endl;
			throw(out_of_range("i"));
		}
	}

	// parens
	else if (parseTree.rule == "factor LPAREN expr RPAREN" or parseTree.rule == "lvalue LPAREN lvalue RPAREN"){
		return typeOf(parseTree.children[1]);
	}

	// pointers
	else if (parseTree.rule == "factor AMP lvalue"){
		if (typeOf(parseTree.children[1]) == "int"){
			return "int*";
		} else {
			cerr << "ERROR: can only reference int" << endl;
			throw(out_of_range("i"));
		}
	} else if (parseTree.rule == "factor STAR factor" or parseTree.rule == "lvalue STAR factor"){
		if (typeOf(parseTree.children[1]) == "int*"){
			return "int";
		} else {
			cerr << "ERROR: can only dereference int*" << endl;
			throw(out_of_range("i"));
		}
	} else if (parseTree.rule == "factor NEW INT LBRACK expr RBRACK"){
		if (typeOf(parseTree.children[3]) == "int"){
			return "int*";
		} else {
			cerr << "ERROR: can only allocate with int" << endl;
			throw(out_of_range("i"));
		}
	}

	// function call with no params
	else if (parseTree.rule == "factor ID LPAREN RPAREN"){
		string proc = parseTree.children[0].tokens[1];
		if (tables.count(proc) <= 0){
			cerr << "ERROR: function " << proc << " not defined" << endl;
			throw(out_of_range("i"));
		} else {
			return "int";
		}
	}

	// function call with params
	else if (parseTree.rule == "factor ID LPAREN arglist RPAREN"){
		string beingCalled = parseTree.children[0].tokens[1];
		if (tables.count(beingCalled) <= 0){
			cerr << "ERROR: function " << beingCalled << " not defined" << endl;
			throw(out_of_range("i"));
		} 
		paramCheck(parseTree.children[2], beingCalled, 0);
		typeOf(parseTree.children[2]);
		return "int";
	}

	// procedure checks
	else if (parseTree.rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		if (typeOf(parseTree.children[9]) != "int"){
			cerr << "ERROR: function does not return int" << endl;
			throw(out_of_range("i"));
		} 
		// check parameters
		typeOf(parseTree.children[3]);
		// check well-typed
		if (typeOf(parseTree.children[6]) == "well-typed" and typeOf(parseTree.children[7]) == "well-typed"){
			return "well-typed";
		} else {
			cerr << "ERROR: not well-typed procedure" << endl;
			throw(out_of_range("i"));
		}
	} else if (parseTree.rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		if (typeOf(parseTree.children[11]) != "int"){
			cerr << "ERROR: function does not return int" << endl;
			throw(out_of_range("i"));
		} 
		// check well-typed
		if (typeOf(parseTree.children[8]) == "well-typed" and typeOf(parseTree.children[9]) == "well-typed" and
			parseTree.children[5].children[0].rule == "type INT"){
			return "well-typed";
		} else {
			cerr << "ERROR: wain is not well-typed" << endl;
			throw(out_of_range("i"));
		}
	}

	// well typed
	// dcls
	else if(parseTree.rule == "dcls"){
		return "well-typed";
	}

	// int assignment
	else if (parseTree.rule == "dcls dcls dcl BECOMES NUM SEMI"){
		if (typeOf(parseTree.children[0]) == "well-typed" and parseTree.children[1].children[0].rule == "type INT"){
			return "well-typed";
		} else {
			cerr << "ERROR: assign integer to non-int variable" << endl;
			throw(out_of_range("i"));
		}
	}

	// pointer assignment
	else if (parseTree.rule == "dcls dcls dcl BECOMES NULL SEMI"){
		if (typeOf(parseTree.children[0]) == "well-typed" and parseTree.children[1].children[0].rule == "type INT STAR"){
			return "well-typed";
		} else {
			cerr << "ERROR: assign NULL to non-pointer variable" << endl;
			throw(out_of_range("i"));
		}
	}

	// just statements
	else if (parseTree.rule == "statements"){
		return "well-typed";
	}

	// not end of statements
	else if (parseTree.rule == "statements statements statement"){
		if (typeOf(parseTree.children[0]) == "well-typed" and typeOf(parseTree.children[1]) == "well-typed"){
			return "well-typed";
		} else {
			cerr << "ERROR: not valid statement" << endl;
			throw(out_of_range("i"));
		}
	} 

	// assignments
	else if (parseTree.rule == "statement lvalue BECOMES expr SEMI"){
		if (typeOf(parseTree.children[0]) == typeOf(parseTree.children[2])){
			return "well-typed";
		} else {
			cerr << "ERROR: invalid type assignment" << endl;
			throw(out_of_range("i"));
		}
	}

	// if statement
	else if (parseTree.rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"){
		if (typeOf(parseTree.children[2]) == "well-typed" and typeOf(parseTree.children[5]) == "well-typed" and typeOf(parseTree.children[9]) == "well-typed"){
			return "well-typed";
		} else {
			cerr << "ERROR: not valid if statement" << endl;
			throw(out_of_range("i"));
		}
	} 

	// while statement
	else if (parseTree.rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE"){
		if (typeOf(parseTree.children[2]) == "well-typed" and typeOf(parseTree.children[5]) == "well-typed"){
			return "well-typed";
		} else {
			cerr << "ERROR: not valid while statement" << endl;
			throw(out_of_range("i"));
		}
	} 

	// println statement
	else if (parseTree.rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
		if (typeOf(parseTree.children[2]) == "int"){
			return "well-typed";
		} else {
			cerr << "ERROR: can only print integer" << endl;
			throw(out_of_range("i"));
		}
	} 

	// deallocation
	else if (parseTree.rule == "statement DELETE LBRACK RBRACK expr SEMI"){
		if (typeOf(parseTree.children[3]) == "int*"){
			return "well-typed";
		} else {
			cerr << "ERROR: can only deallocate pointer" << endl;
			throw(out_of_range("i"));
		}
	}

	// tests
	// equality
	else if (parseTree.rule == "test expr EQ expr" or parseTree.rule == "test expr NE expr" or parseTree.rule == "test expr LT expr" or
		parseTree.rule == "test expr LE expr" or parseTree.rule == "test expr GE expr" or parseTree.rule == "test expr GT expr"){
		if (typeOf(parseTree.children[0]) == typeOf(parseTree.children[2])){
			return "well-typed";
		} else {
			cerr << "ERROR: can only compare between variables with the same type" << endl;
			throw(out_of_range("i"));
		}
	}

	// inequliaty
	else if (parseTree.rule == "test expr NE expr"){
		if (typeOf(parseTree.children[0]) == typeOf(parseTree.children[2])){
			return "well-typed";
		} else {
			cerr << "ERROR: can only compare between variables with the same type" << endl;
			throw(out_of_range("i"));
		}
	}

	else {return "";}
}	

// check if the LHS is terminal
bool terminal(string s){
	vector<string> terminals = {"BOF", "BECOMES", "COMMA", "ELSE", "EOF", "EQ", 
	"GE", "GT", "ID", "IF", "INT", "LBRACE", "LE", "LPAREN", "LT", "MINUS", "NE", 
	"NUM", "PCT", "PLUS", "PRINTLN", "RBRACE", "RETURN", "RPAREN", "SEMI", "SLASH", 
	"STAR", "WAIN", "WHILE", "AMP", "LBRACK", "RBRACK", "NEW", "DELETE", "NULL"};

	int size = terminals.size();
	for (int i = 0; i < size; i++){
		if (s == terminals[i]){
			return true;
		}
	}

	return false;
}

// build the parse tree
Tree buildTree(string line){

	Tree Node;
	Node.rule = line;
	stringstream iss(line);
	string temp;

	while (iss >> temp){
		Node.tokens.emplace_back(temp);
	}

	if (!terminal(Node.tokens[0])){
		int size = Node.tokens.size();
		for (int i = 0; i < size-1; i++){
			getline(cin, line);
			Node.children.emplace_back(buildTree(line));
		}
	}

	return Node;
}

int curoffset = 0;

void symbolTable(const Tree &parseTree){

	for (auto it = parseTree.children.begin(); it != parseTree.children.end(); it++){
		// new procedure
		if (it->rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
			string procedure = it->children[1].tokens[1];
			curoffset = 0;
			if (tables.count(procedure) > 0){
				cerr << "ERROR: duplicate procedure " << procedure << endl;
				throw(out_of_range("i"));
			} else {
				pair<vector<string>, map<string, pair<string, int> > > sigs;
				tables[procedure] = sigs;
			}
			currentActive = procedure;
		} else if (it->rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
			curoffset = 0;
			string procedure = it->children[1].tokens[1];
			if (tables.count(procedure) > 0){
				cerr << "ERROR: duplicate procedure " << procedure << endl;
				throw(out_of_range("i"));
			} else {
				pair<vector<string>, map<string, pair<string, int> > > sigs;
				tables[procedure] = sigs;
			}
			currentActive = procedure;

			// check params and return type
			if (it->children[3].children[0].rule == "type INT"){
				string type = "int";
				tables[currentActive].first.emplace_back(type);
			} else if (it->children[3].children[0].rule == "type INT STAR"){
				string type = "int*";
				tables[currentActive].first.emplace_back(type);
			}

			string id = it->children[5].children[1].tokens[1];
			for (auto it = tables[currentActive].first.begin(); it != tables[currentActive].first.end();
				it++){
				if (id == *it){
					cerr << "ERROR: parameter " << id << " defined twice" << endl;
					throw(out_of_range("i"));
				}
			}
			if (it->children[5].children[0].rule == "type INT"){
				string type = "int";

				tables[currentActive].first.emplace_back(type);
			} else if (it->children[5].children[0].rule == "type INT STAR"){
				string type = "int*";
				tables[currentActive].first.emplace_back(type);
			}
		}

		// declare variable
		else if (it->rule == "dcl type ID"){
			string lexeme = it->children[1].tokens[1];
			
			// check type
			string type;
			if (it->children[0].rule == "type INT STAR"){
				type = it->children[0].children[0].tokens[1] + it->children[0].children[1].tokens[1];
			} else {
				type = it->children[0].children[0].tokens[1];
			}

			// push type id pair into tables
			if (tables[currentActive].second.count(lexeme) > 0){
				cerr << "ERROR: duplicate variable " << lexeme << endl;
				throw(out_of_range("i"));
			} else {
				tables[currentActive].second[lexeme].first = type;
				tables[currentActive].second[lexeme].second = curoffset;
				curoffset -= 4;
			}			
		}

		// check use before declaration
		else if (it->rule == "factor ID" or it->rule == "lvalue ID"){
			string lexeme = it->children[0].tokens[1];
			if (tables[currentActive].second.count(lexeme) <= 0){
				cerr << "ERROR: variable " << lexeme << " use before declaration" << endl;
				throw(out_of_range("i"));
			}
		}

		// param list
		else if (it->rule == "paramlist dcl" or it->rule == "paramlist dcl COMMA paramlist"){
			string sig = it->children[0].children[0].rule;
			string id = it->children[0].children[1].tokens[1];
			for (auto it = tables[currentActive].first.begin(); it != tables[currentActive].first.end();
				it++){
				if (id == *it){
					cerr << "ERROR: parameter " << id << " defined twice" << endl;
					throw(out_of_range("i"));
				}
			}
			if (sig == "type INT"){
				sig = "int";
				tables[currentActive].first.emplace_back(sig);
			} else if (sig == "type INT STAR"){
				sig = "int*";
				tables[currentActive].first.emplace_back(sig);
			}
		}

		// valid procedure call
		else if (it->rule == "factor ID LPAREN RPAREN" or
			it->rule == "factor ID LPAREN arglist RPAREN"){
			string procedure = it->children[0].tokens[1];
			if (tables.count(procedure) <= 0){
				cerr << "ERROR: procedure " << procedure << " called before declared" << endl;
				throw(out_of_range("i"));
			} else if (tables[currentActive].second.count(procedure) > 0){
				cerr << "ERROR: variable with the same name " << procedure << " declared" << endl;
				throw(out_of_range("i"));
			} else {
				for (auto it = tables[currentActive].first.begin(); it != tables[currentActive].first.end(); it++){
					if (*it == procedure){
						cerr << "ERROR: parameter with the same name " << procedure << " declared" << endl;
						throw(out_of_range("i"));
					}
				}
			}
		}

		symbolTable(*it);
	}
}

/*void printSymbols(){
	for(auto it = tables.begin(); it != tables.end(); it++){
		if (it->first != "wain"){
			cerr << it->first;
			bool first = true;
			for (auto it2 = it->second.first.begin(); it2 != it->second.first.end(); it2++){
				if (first){
					cerr << " ";
					first = false;
				}
				if (it2 + 1 == it->second.first.end()){
					cerr << *it2; // last one
				} else {
					cerr << *it2 << " ";
				}
			}
			cerr << endl;
			for (auto it1 = it->second.second.begin(); it1 != it->second.second.end(); it1++){
				cerr << it1->first << " " << it1->second << endl;
			}
			cerr << "end" << endl;
			cerr << endl;
		}
	}

	cerr << "wain";
	bool first = true;
	for (auto it2 = tables["wain"].first.begin(); it2 != tables["wain"].first.end(); it2++){
		if (first){
			cerr << " ";
			first = false;
		}
		if (it2 + 1 == tables["wain"].first.end()){
			cerr << *it2; // last one
		} else {
			cerr << *it2 << " ";
		}
	}
	cerr << endl;
	for (auto it1 = tables["wain"].second.begin(); it1 != tables["wain"].second.end(); it1++){
		cerr << it1->first << " " << it1->second << endl;
	}
}*/

void checkType(Tree parseTree){
	for (auto it = parseTree.children.begin(); it != parseTree.children.end(); it++){
		typeOf(*it);
		checkType(*it);
	}
}

void push(int reg) {
	cout << "sw $" << reg <<", -4($30)" << endl;
	cout << "sub $30, $30, $4" << endl;
	return;
}

void pop(int reg){
	cout << "add $30, $30, $4" << endl;
	cout << "lw $" << reg << ", -4($30)" << endl;
	return;
}

void wainPro(const Tree& parseTree) {
	cout << "; wain prologue" << endl;
	// conventions
	cout << ".import print" << endl;

	cout << ".import init" << endl;
	cout << ".import new" << endl;
	cout << ".import delete" << endl;
	cout << "lis $4" << endl;
	cout << ".word 4" << endl;
	cout << "lis $10" << endl;
	cout << ".word print" << endl;
	cout << "lis $11" << endl;
	cout << ".word 1" << endl;

	// setup frame pointer
	cout << "sub $29, $30, $4" << endl;

	// push $1 and $2 into stack
	cout << "sw $1, -4($30)" << endl;
	cout << "sw $2, -8($30)" << endl;
	cout << "sub $30, $30, $4" << endl;
	cout << "sub $30, $30, $4" << endl;

	// init
	if (parseTree.children[3].children[0].rule == "type INT"){
		cout << "add $2, $0, $0" << endl;
	}

	push(31);
	cout << "lis $5" << endl;
	cout << ".word init" << endl;
	cout << "jalr $5" << endl;
	pop(31);

	cout << "; end wain prologue" << endl;

	return;
}

void wainEpi(const Tree& parseTree){
	cout << "; wain epilogue" << endl;
	cout << "add $30, $30, $4" << endl;
	cout << "add $30, $30, $4" << endl;
	cout << "jr $31" << endl;
	return;
}

void procPro(const Tree& parseTree){
	// print procedure name
	cout << "F" << currentActive << ":" << endl;

	// setup frame pointer
	cout << "sub $29, $30, $4" << endl;

	// declarations
	generateCode(parseTree.children[6]);

	// save all registers being used
	push(1);
	push(2);
	push(5);
	push(6);
	push(7);

	return;
}

void procEpi(const Tree& parseTree){
	// restore registers
	cout << "; epilogue" << endl;
	pop(7);
	pop(6);
	pop(5);
	pop(2);
	pop(1);

	// restore stack pointer
	cout << "add $30, $29, $4" << endl;
	cout << "jr $31" << endl;
}
	
void addOffset(string procedure){
	int toAdd = tables[procedure].first.size() * 4;

	for (auto it = tables[procedure].second.begin(); it != tables[procedure].second.end(); it++){
		it->second.second += toAdd;
	}
}

void generateCode(const Tree& parseTree) {
	/*cerr << "start generate" << endl;*/
	if (parseTree.rule == "start BOF procedures EOF") {
		generateCode(parseTree.children[1]);
	} else if (parseTree.rule == "procedures main") {
		generateCode(parseTree.children[0]);
	} else if (parseTree.rule == "main INT WAIN LPAREN dcl COMMA dcl RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE") {
		// main procedure
		currentActive = "wain";

		// prologue for main
		wainPro(parseTree);

		generateCode(parseTree.children[8]);
		generateCode(parseTree.children[9]);
		generateCode(parseTree.children[11]);

		// epilogue for main
		wainEpi(parseTree);
	} else if (parseTree.rule == "dcls") {
		return;
	} else if (parseTree.rule == "dcl type ID"){
		return;
	} else if (parseTree.rule == "statements") {
		return;
	} else if (parseTree.rule == "expr term"){
		generateCode(parseTree.children[0]);
	} else if (parseTree.rule == "term factor"){
		generateCode(parseTree.children[0]);
	} else if (parseTree.rule == "factor ID") {
		string id = parseTree.children[0].tokens[1];
		int offset = tables[currentActive].second[id].second;
		cout << "lw $3, " << offset << "($29)" << endl;
		return;
	} else if (parseTree.rule == "factor LPAREN expr RPAREN"){
		generateCode(parseTree.children[1]);
	}

	// integer operators
	else if (parseTree.rule == "expr expr PLUS term"){
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		// check point arithmetic
		if (typeOf(parseTree.children[0]) == "int*" and typeOf(parseTree.children[2]) == "int"){
			cout << "mult $3, $4" << endl;
			cout << "mflo $3" << endl;
		}
		pop(5);
		if (typeOf(parseTree.children[0]) == "int" and typeOf(parseTree.children[2]) == "int*"){
			cout << "mult $5, $4" << endl;
			cout << "mflo $5" << endl;
		}
		cout << "add $3, $5, $3" << endl;
		return;
	} else if (parseTree.rule == "expr expr MINUS term"){
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		// check point arithmetic
		if (typeOf(parseTree.children[0]) == "int*" and typeOf(parseTree.children[2]) == "int"){
			cout << "mult $3, $4" << endl;
			cout << "mflo $3" << endl;
		}
		pop(5);
		cout << "sub $3, $5, $3" << endl;
		if (typeOf(parseTree.children[0]) == "int*" and typeOf(parseTree.children[2]) == "int*"){
			cout << "div $3, $4" << endl;
			cout << "mflo $3" << endl;	
		}
		return;
	} else if (parseTree.rule == "term term STAR factor") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		cout << "mult $3, $5" << endl;
		cout << "mflo $3" << endl;
		return;
	} else if (parseTree.rule == "term term SLASH factor") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		cout << "div $5, $3" << endl;
		cout << "mflo $3" << endl;
		return;
	} else if (parseTree.rule == "term term PCT factor"){
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		cout << "div $5, $3" << endl;
		cout << "mfhi $3" << endl;
		return;
	} else if (parseTree.rule == "factor NUM"){
		string num = parseTree.children[0].tokens[1];
		cout << "lis $3" << endl;
		cout << ".word " << num << endl;
		return;
	}

	// println statement
	else if (parseTree.rule == "statements statements statement"){
		generateCode(parseTree.children[0]);
		generateCode(parseTree.children[1]);
	} else if (parseTree.rule == "statement PRINTLN LPAREN expr RPAREN SEMI"){
		generateCode(parseTree.children[2]);
		cout << "add $1, $3, $0" << endl;
		push(31);
		cout << "lis $5" << endl;
		cout << ".word print" << endl;
		cout << "jalr $5" << endl;
		pop(31);
	}

	// variable declarations and assignment statements
	else if (parseTree.rule == "dcls dcls dcl BECOMES NUM SEMI") {
		generateCode(parseTree.children[0]);
		// num to be assigned
		string num = parseTree.children[3].tokens[1];
		cout << "lis $3" << endl;
		cout << ".word " << num << endl;

		// lvalue to be stored
		string id = parseTree.children[1].children[1].tokens[1];
		int offset = tables[currentActive].second[id].second;
		cout << "sw $3, " << offset << "($29)" << endl;
		cout << "sub $30, $30, $4" << endl;
		return;
	} else if (parseTree.rule == "statement lvalue BECOMES expr SEMI") {
		generateCode(parseTree.children[2]);

		Tree temp = parseTree.children[0];
		if (parseTree.children[0].rule == "lvalue LPAREN lvalue RPAREN"){
			while(temp.rule == "lvalue LPAREN lvalue RPAREN"){
				temp = temp.children[0];
			}
		}

		if (temp.rule == "lvalue ID"){
			string id = temp.children[0].tokens[1];
			int offset = tables[currentActive].second[id].second;
			cout << "sw $3, " << offset << "($29)" << endl;
			return;
		} else if (temp.rule == "lvalue STAR factor"){
			push(3);
			generateCode(temp.children[1]);
			pop(5);
			cout << "sw $5, 0($3)" << endl;
			return;
		}
	}

	// while loops
	else if (parseTree.rule == "statement WHILE LPAREN test RPAREN LBRACE statements RBRACE") {
		int curr = whileCount;
		whileCount++;
		cout << "loop" << curr << ":" << endl;
		generateCode(parseTree.children[2]);
		cout << "beq $3, $0, endwhile" << curr << endl;
		generateCode(parseTree.children[5]);
		cout << "beq $0, $0, loop" << curr << endl;
		cout << "endwhile" << curr << ":" << endl;
		return;
	}

	// if statement
	else if (parseTree.rule == "statement IF LPAREN test RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE") {
		int curr = ifCount;
		ifCount++;
		generateCode(parseTree.children[2]);
		cout << "beq $3, $0, else" << curr << endl;
		generateCode(parseTree.children[5]);
		cout << "beq $0, $0, endif" << curr << endl;
		cout << "else" << curr << ":" << endl;
		generateCode(parseTree.children[9]);
		cout << "endif" << curr << ":" << endl;
		return;
	}

	// comparison
	else if (parseTree.rule == "test expr LT expr") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		// check type
		if (typeOf(parseTree.children[0]) == "int*"){
			cout << "sltu $3, $5, $3" << endl;
		} else {
			cout << "slt $3, $5, $3" << endl;
		}		
		return;
	} else if (parseTree.rule == "test expr EQ expr") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		// check type
		if (typeOf(parseTree.children[0]) == "int*"){
			cout << "sltu $6, $3, $5" << endl;
			cout << "sltu $7, $5, $3" << endl;
		} else {
			cout << "slt $6, $3, $5" << endl;
			cout << "slt $7, $5, $3" << endl;
		}		
		cout << "add $3, $6, $7" << endl;
		cout << "sub $3, $11, $3" << endl;
		return;
	} else if (parseTree.rule == "test expr NE expr"){
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		// check type
		if (typeOf(parseTree.children[0]) == "int*"){
			cout << "sltu $6, $3, $5" << endl;
			cout << "sltu $7, $5, $3" << endl;
		} else {
			cout << "slt $6, $3, $5" << endl;
			cout << "slt $7, $5, $3" << endl;
		}
		cout << "add $3, $6, $7" << endl;
		return;
	} else if (parseTree.rule == "test expr GT expr") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		// check type
		if (typeOf(parseTree.children[0]) == "int*"){
			cout << "sltu $3, $3, $5" << endl;
		} else {
			cout << "slt $3, $3, $5" << endl;
		}
		return;
	} else if (parseTree.rule == "test expr LE expr") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		// check type
		if (typeOf(parseTree.children[0]) == "int*"){
			cout << "sltu $3, $3, $5" << endl;
		} else {
			cout << "slt $3, $3, $5" << endl;
		}		
		cout << "sub $3, $11, $3" << endl;
		return;
	} else if (parseTree.rule == "test expr GE expr") {
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
		pop(5);
		// check type
		if (typeOf(parseTree.children[0]) == "int*"){
			cout << "sltu $3, $5, $3" << endl;
		} else {
			cout << "slt $3, $5, $3" << endl;
		}
		
		cout << "sub $3, $11, $3" << endl;
		return;
	}

	// pointers
	else if (parseTree.rule == "dcls dcls dcl BECOMES NULL SEMI"){
		generateCode(parseTree.children[0]);
		string id = parseTree.children[1].children[1].tokens[1];
		int offset = tables[currentActive].second[id].second;
		cout << "add $3, $11, $0" << endl;
		cout << "sw $3, " << offset << "($29)" << endl;
		cout << "sub $30, $30, $4" << endl;
		return;
	} else if (parseTree.rule == "factor NULL"){
		cout << "add $3, $11, $0" << endl;
		return;
	} else if (parseTree.rule == "factor AMP lvalue") {

		Tree temp = parseTree.children[1];

		while (temp.rule == "lvalue LPAREN lvalue RPAREN"){
			temp = temp.children[1];
		}

		if (temp.rule == "lvalue ID"){
			cout << "lis $3" << endl;
			string id = temp.children[0].tokens[1];
			int offset = tables[currentActive].second[id].second;
			cout << ".word " << offset << endl;
			cout << "add $3, $3, $29" << endl;
			return;
		} else if (parseTree.children[1].rule == "lvalue STAR factor"){
			generateCode(parseTree.children[1]);
		}
	} else if (parseTree.rule == "factor STAR factor") {
		generateCode(parseTree.children[1]);
		cout << "lw $3, 0($3)" << endl;
		return; 
	}

	else if (parseTree.rule == "factor NEW INT LBRACK expr RBRACK"){
		generateCode(parseTree.children[3]);
		cout << "add $1, $3, $0" << endl;
		push(31);
		cout << "lis $5" << endl;
		cout << ".word new" << endl;
		cout << "jalr $5" << endl;
		pop(31);
		cout << "bne $3, $0, 1" << endl;
		cout << "add $3, $11, $0" << endl;
		return;
	} else if (parseTree.rule == "statement DELETE LBRACK RBRACK expr SEMI"){
		generateCode(parseTree.children[3]);
		int curr = skipCount;
		skipCount++;
		cout << "beq $3, $11, skipDelete" << curr << endl;
		cout << "add $1, $3, $0" << endl;
		push(31);
		cout << "lis $5" << endl;
		cout << ".word delete" << endl;
		cout << "jalr $5" << endl;
		pop(31);
		cout << "skipDelete" << curr << ":" << endl;
		return;
	}

	// calling other procedures
	else if (parseTree.rule == "procedures procedure procedures"){
		generateCode(parseTree.children[1]);
		generateCode(parseTree.children[0]);
	} else if (parseTree.rule == "procedure INT ID LPAREN params RPAREN LBRACE dcls statements RETURN expr SEMI RBRACE"){
		currentActive = parseTree.children[1].tokens[1];
		// procedure with params
		if (parseTree.children[3].rule == "params paramlist"){
			addOffset(currentActive);
		}
		procPro(parseTree); // caller save
		generateCode(parseTree.children[7]);
		generateCode(parseTree.children[9]);
		procEpi(parseTree);
		return;
	} else if (parseTree.rule == "factor ID LPAREN RPAREN") {
		push(29);
		push(31);
		string id = parseTree.children[0].tokens[1];
		cout << "lis $5" << endl;
		cout << ".word " << "F" << id << endl;
		cout << "jalr $5" << endl;
		pop(31);
		pop(29);
		return;
	}

	// procedure with params
	else if (parseTree.rule == "factor ID LPAREN arglist RPAREN"){
		push(29);
		push(31);
		generateCode(parseTree.children[2]);
		string id = parseTree.children[0].tokens[1];
		cout << "lis $5" << endl;
		cout << ".word " << "F" << id << endl;
		cout << "jalr $5" << endl;

		// pop back 
		int offset = tables[id].first.size() * 4;
		cout << "lis $5" << endl;
		cout << ".word " << offset << endl;
		cout << "add $30, $30, $5" << endl;
		pop(31);
		pop(29);
	} else if (parseTree.rule == "arglist expr") {
		generateCode(parseTree.children[0]);
		push(3);
	} else if (parseTree.rule == "arglist expr COMMA arglist"){
		generateCode(parseTree.children[0]);
		push(3);
		generateCode(parseTree.children[2]);
	}
}

int main(){
	Tree parseTree;
	
	try {
		string line;
		getline(cin, line);
		parseTree = buildTree(line);
		symbolTable(parseTree);
		/*checkType(parseTree);*/
		generateCode(parseTree);
		/*printSymbols();*/
	} catch(...){
		return 1;
	}
}
