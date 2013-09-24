<?php
class ExpressionSyntaxError extends Exception {}
class ExpressionParseError extends Exception {}
class ExpressionRuntimeError extends Exception {}

define('T_NUMBER',      1);  // number
define('T_IDENT',       2);  // constant
define('T_POPEN',       8);  // (
define('T_PCLOSE',      16); // )
define('T_OPERATOR',    64); // operator
define('T_PLUS',        65); // +
define('T_MINUS',       66); // -
define('T_TIMES',       67); // *
define('T_DIV',         68); // /
define('T_UNARY_PLUS',  71); // + as signed
define('T_UNARY_MINUS', 72); // - as signed

class InfixExpressionParser
{
    const ST_1 = 1, // waiting for operand
          ST_2 = 2; // waiting for operator

    protected $syntax_only;

    protected $scanner, $state = self::ST_1;
    protected $queue, $stack;

    public function __construct($term, $pattern = null, $syntax_only = false)
    {
        $this->scanner = new InfixExpressionScanner($term, $pattern);

        $this->syntax_only = $syntax_only;

        // alloc
        $this->queue = array();
        $this->stack = array();

        // create queue
        while (($t = $this->scanner->next()) !== false) {
            $this->handle($t);
        }

        // When there are no more tokens to read:
        // While there are still operator tokens in the stack:
        while ($t = array_pop($this->stack)) {
            if ($t->type === T_POPEN || $t->type === T_PCLOSE) {
                throw new ExpressionParseError('括号不匹配');
            }

            $this->queue[] = $t;
        }
    }

    public function reduce(ExpressionContext $ctx)
    {
        $this->stack = array();
        $len = 0;

        // While there are input tokens left
        // Read the next token from input.
        while ($t = array_shift($this->queue)) {
            switch ($t->type) {
                case T_NUMBER:
                case T_IDENT:
                    // a constant value determined
                    if ($t->type === T_IDENT) {
                        if ($this->syntax_only) {
                            // if syntax only, assume all contstants to be 1
                            $t = new ExpressionToken(T_NUMBER, 1);
                        } else {
                            $t = new ExpressionToken(T_NUMBER, $ctx->cs($t->value));
                        }
                    }

                    // If the token is a value or identifier
                    // Push it onto the stack.
                    $this->stack[] = $t;
                    ++$len;
                    break;

                case T_PLUS:
                case T_MINUS:
                case T_UNARY_PLUS:
                case T_UNARY_MINUS:
                case T_TIMES:
                case T_DIV:
                    // It is known a priori that the operator takes n arguments.
                    $na = $this->argc($t);

                    // If there are fewer than n values on the stack
                    if ($len < $na) {
                        throw new ExpressionRuntimeError('操作符没有足够参数 "'
                            . $t->value . '" (' . $na . ' -> ' . $len . ')');
                    }

                    $rhs = array_pop($this->stack);
                    $lhs = null;

                    if ($na > 1) {
                        $lhs = array_pop($this->stack);
                    }

                    $len -= $na - 1;

                    // Push the returned results, if any, back onto the stack.
                    // if syntax only, assume all results to be 1
                    if ($this->syntax_only) {
                        $this->stack[] = new ExpressionToken(T_NUMBER, 1);
                    } else {
                        $this->stack[] = new ExpressionToken(T_NUMBER, $this->op($t->type, $lhs, $rhs));
                    }
                    break;

                default:
                    throw new ExpressionRuntimeError('无效符号 "' . $t->value . '"');
            }
        }

        // If there is only one value in the stack
        // That value is the result of the calculation.
        if (count($this->stack) === 1) {
            return array_pop($this->stack)->value;
        }

        // If there are more values in the stack
        // (Error) The user input has too many values.
        throw new ExpressionRuntimeError('操作数过多');
    }

    protected function op($op, $lhs, $rhs)
    {
        if ($lhs !== null) {
            $lhs = $lhs->value;
            $rhs = $rhs->value;

            switch ($op) {
                case T_PLUS:
                    return $lhs + $rhs;

                case T_MINUS:
                    return $lhs - $rhs;

                case T_TIMES:
                    return $lhs * $rhs;

                case T_DIV:
                    if ($rhs === 0) {
                        throw new ExpressionRuntimeError('除数不能为0');
                    }
                    return $lhs / $rhs;
            }

            return 0;
        }

        switch ($op) {
            case T_UNARY_MINUS:
                return -$rhs->value;

            case T_UNARY_PLUS:
                return +$rhs->value;
        }
    }

    protected function argc(ExpressionToken $t)
    {
        switch ($t->type) {
            case T_PLUS:
            case T_MINUS:
            case T_TIMES:
            case T_DIV:
                return 2;
        }

        return 1;
    }

    protected function handle(ExpressionToken $t)
    {
        switch ($t->type) {
            case T_NUMBER:
            case T_IDENT:
                // If the token is a number (identifier), then add it to the output queue.
                $this->queue[] = $t;
                $this->state = self::ST_2;
                break;

                // If the token is an operator, op1, then:
            case T_PLUS:
            case T_MINUS:
            case T_UNARY_PLUS:
            case T_UNARY_MINUS:
            case T_TIMES:
            case T_DIV:
                while (!empty($this->stack)) {
                    $s = end($this->stack);

                    // While there is an operator token, o2, at the top of the stack
                    // op1 is left-associative and its precedence is less than or equal to that of op2,
                    // or op1 has precedence less than that of op2,
                    // Let + be right associative.
                    // The differing operator priority decides pop / push
                    // If 2 operators have equal priority then associativity decides.
                    switch ($s->type) {
                        default:
                            break 2;

                        case T_PLUS:
                        case T_MINUS:
                        case T_UNARY_PLUS:
                        case T_UNARY_MINUS:
                        case T_TIMES:
                        case T_DIV:
                            $p1 = $this->preced($t);
                            $p2 = $this->preced($s);

                            if (!(($this->assoc($t) === 1 && ($p1 <= $p2)) || ($p1 < $p2))) {
                                break 2;
                            }

                            // Pop o2 off the stack, onto the output queue;
                            $this->queue[] = array_pop($this->stack);
                    }
                }

                // push op1 onto the stack.
                $this->stack[] = $t;
                $this->state = self::ST_1;
                break;

            case T_POPEN:
                // If the token is a left parenthesis, then push it onto the stack.
                $this->stack[] = $t;
                $this->state = self::ST_1;
                break;

                // If the token is a right parenthesis:
            case T_PCLOSE:
                $pe = false;

                // Until the token at the top of the stack is a left parenthesis,
                // pop operators off the stack onto the output queue
                while ($t = array_pop($this->stack)) {
                    if ($t->type === T_POPEN) {
                        // Pop the left parenthesis from the stack, but not onto the output queue.
                        $pe = true;
                        break;
                    }

                    $this->queue[] = $t;
                }

                // If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
                if ($pe !== true) {
                    throw new ExpressionParseError('无效括号');
                }

                $this->state = self::ST_2;
                break;

            default:
                throw new ExpressionParseError('无效符号 "' . $t->value . '"');
        }
    }

    protected function assoc(ExpressionToken $t)
    {
        switch ($t->type) {
            case T_TIMES:
            case T_DIV:
            case T_PLUS:
            case T_MINUS:
                return 1; //ltr

            case T_UNARY_PLUS:
            case T_UNARY_MINUS:
                return 2; //rtl
        }

        return 0;
    }

    protected function preced(ExpressionToken $t)
    {
        switch ($t->type) {
            case T_UNARY_PLUS:
            case T_UNARY_MINUS:
                return 4;

            case T_TIMES:
            case T_DIV:
                return 2;

            case T_PLUS:
            case T_MINUS:
                return 1;
        }

        return 0;
    }

    public function parse(ExpressionContext $ctx = null)
    {
        return $this->reduce($ctx ? $ctx : new ExpressionContext);
    }

    public function getScanner() { return $this->scanner; }
}

class InfixExpressionScanner
{
    const ERR_EMPTY = '不能为空 "%s"',
          ERR_MATCH = '无效符号 "%s"';

    protected $pattern = '/^([\+\-\*\/\(\)]|\d*\.\d+|\d+\.\d*|\d+|[a-z_A-Z]+[a-z_A-Z0-9]*|[ \t]+)/';

    protected $tokens = array(0);

    protected $constants = array();

    protected $lookup = array(
        '+' => T_PLUS,
        '-' => T_MINUS,
        '*' => T_TIMES,
        '/' => T_DIV,
        '(' => T_POPEN,
        ')' => T_PCLOSE,
    );

    public function __construct($input, $pattern = null)
    {
        if ($pattern !== null) {
            $this->pattern = $pattern;
        }

        $prev = new ExpressionToken(T_OPERATOR, 'noop');

        while (trim($input) !== '') {
            if (!preg_match($this->pattern, $input, $match)) {
                throw new ExpressionSyntaxError(sprintf(self::ERR_MATCH, substr($input, 0, 10)));
            }

            if (empty($match[1]) && $match[1] !== '0') {
                throw new ExpressionSyntaxError(sprintf(self::ERR_EMPTY, substr($input, 0, 10)));
            }

            $input = substr($input, strlen($match[1]));

            if (($value = trim($match[1])) === '') {
                continue;
            }

            if (is_numeric($value)) {
                $this->tokens[] = $prev = new ExpressionToken(T_NUMBER, (float) $value);
                continue;
            }

            switch ($type = isset($this->lookup[$value]) ? $this->lookup[$value] : T_IDENT) {
                case T_PLUS:
                    if ($prev->type & T_OPERATOR || $prev->type == T_POPEN) $type = T_UNARY_PLUS;
                    break;

                case T_MINUS:
                    if ($prev->type & T_OPERATOR || $prev->type == T_POPEN) $type = T_UNARY_MINUS;
                    break;

                case T_PCLOSE:
                    if ($prev->type & T_POPEN) {
                        throw new ExpressionSyntaxError(sprintf(self::ERR_EMPTY, substr($input, 0, 10)));
                    }
                    break;

                case T_IDENT:
                    $this->constants[] = $value;
                    break;
            }

            $this->tokens[] = $prev = new ExpressionToken($type, $value);
        }
    }

    public function next() { return next($this->tokens); }

    public function getConstants() { return array_unique($this->constants); }
}

class ExpressionToken
{
    public $type, $value;

    public function __construct($type, $value)
    {
        $this->type  = $type;
        $this->value = $value;
    }
}

class ExpressionContext
{
    protected $cst = array();

    public function cs($name)
    {
        if (!isset($this->cst[$name])) {
            throw new ExpressionRuntimeError('常量未定义 "' . $name . '"');
        }

        return $this->cst[$name];
    }

    public function def($name, $value = null)
    {
        if (is_numeric($value)) {
            $this->cst[$name] = (float) $value;
        } else {
            throw new ExpressionSyntaxError('值必须为数');
        }
    }
}
