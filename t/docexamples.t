#! /usr/bin/perl
#
# This contains most examples from the documentation and checks
# that they do what they are supposed to do.

use strict;
use warnings;
use Data::Dumper;
use Test::More tests => 124;

use SQL::Yapp
    table_prefix => 'T'
  , write_dialect => 'mysql'
  , quote_identifier => sub {
        join('.', map { "`$_`" } grep { defined($_) } @_)
    }
  , quote => sub {
        qq{'$_[0]'}
    }
  #, debug => 1
;

######################################################################
# A few initial tests:
isnt(undef, ''); # just to be sure

is(sqlTable{ blah.blup },
          q{`blah`.`Tblup`});

is(sqlExpr{ 5 + blah },
         q{('5' + `blah`)});

is(sqlExpr{ "test" },
         q{'test'});

is(sql{ SELECT b IN (SELECT 2) },
     q{SELECT `b` IN (SELECT '2')});

my $xa= 'a';

is(sql{SELECT $xa FROM bar},
     q{SELECT 'a' FROM `Tbar`});

is(sql{SELECT .$xa FROM bar},
     q{SELECT `a` FROM `Tbar`});

######################################################################
# The following are systematically all examples from the documentation:

####
# SYNOPSIS
{
    my $first_name= "Peter";
    my $q= sql{
        SELECT surname FROM customer WHERE first_name = $first_name
    };
    is($q, q{SELECT `surname` FROM `Tcustomer` WHERE `first_name` = 'Peter'});
}

{
    my $column= 'surname';
    my $q= sql{
        SELECT customer.$column FROM customer WHERE first_name = 'John'
    };
    is($q, q{SELECT `Tcustomer`.`surname` FROM `Tcustomer` WHERE `first_name` = 'John'});
}

{
    my $sur= 1;
    my $q= sql{
        SELECT .{ $sur ? 'surname' : 'first_name' } FROM customer
    };
    is($q, q{SELECT `surname` FROM `Tcustomer`});
}

{
    my @val= ( 1, 2, 3 );
    my $q= sql{
        SELECT @val
    };
    is($q, q{SELECT '1', '2', '3'});
}

{
    my @col= ( 'surname', 'first_name' );
    my $q= sql{
        SELECT .@col FROM customer
    };
    is($q, q{SELECT `surname`, `first_name` FROM `Tcustomer`});
}

{
    my @tab= ( 'friends', 'enemies' );
    my $q= sql{
        SELECT @tab.surname FROM @tab
    };
    is($q, q{SELECT `Tfriends`.`surname`, `Tenemies`.`surname` FROM `Tfriends`, `Tenemies`});
}

{
    my @col= ( 'surname', 'first_name' );
    my @tab= ( 'friends', 'enemies' );
    my $q= sql{
        SELECT @tab.@col FROM @tab
    };
    is($q, q{SELECT `Tfriends`.`surname`, `Tfriends`.`first_name`, }.
           q{`Tenemies`.`surname`, `Tenemies`.`first_name` FROM `Tfriends`, `Tenemies`});
}

{
    my $sur= 1;
    my $q= sql{
        SELECT surname FROM customer
        WHERE
           {$sur ?
               sql{ surname    LIKE '%foo%' }
           :   sql{ first_name LIKE '%bar%' }
           }
    };
    is($q, q{SELECT `surname` FROM `Tcustomer` WHERE `surname` LIKE '%foo%'});
}

{
    my $expr= sqlExpr{         (b * 6) = COALESCE(c, d)        };
    is ($expr, q{(`b` * '6') = COALESCE(`c`, `d`)});

    my $tab=  sqlTable{        bar                             };
    is ($tab, q{`Tbar`});

    my $col=  sqlColumn{       $tab.name                       };
    is ($col, q{`Tbar`.`name`});

    my $join= sqlJoin{         LEFT JOIN foo ON $col == foo.id };
    is ($join, q{LEFT JOIN `Tfoo` ON `Tbar`.`name` = `Tfoo`.`id`});

    my @ordr= sqlOrder{        a, b DESC                       };
    is (join(', ',@ordr), q{`a`, `b` DESC});

    my $stmt= sqlStmt{ SELECT $col
                       FROM $tab
                       Join $join
                       WHERE $expr
                       ORDER BY @ordr };

    is ($stmt, q{SELECT `Tbar`.`name` FROM `Tbar` }.
               q{LEFT JOIN `Tfoo` ON `Tbar`.`name` = `Tfoo`.`id` }.
               q{WHERE (`b` * '6') = COALESCE(`c`, `d`) ORDER BY `a`, `b` DESC});

    my $type= sqlType{         INT(10) };
    is ($type, q{INT (10)});

    my $spec= sqlColumnSpec {  $type NOT NULL DEFAULT 17 };
    is ($spec, q{INT (10) NOT NULL DEFAULT '17'});

    my @to=   sqlTableOption{  ENGINE innodb
                               DEFAULT CHARACTER SET utf8
                            };
    is (join(" ",@to), q{ENGINE `innodb` DEFAULT CHARACTER SET `utf8`});

    my $stm2= sqlStmt{         CREATE TABLE foo ( col1 $spec ) @to };
    is ($stm2, q{CREATE TABLE `Tfoo` (`col1` INT (10) NOT NULL DEFAULT '17') }.
               q{ENGINE `innodb` DEFAULT CHARACTER SET `utf8`});
}

{
    my %new_value= (
        first_name => 'John',
    );
    my $q= sql{
        UPDATE customer SET %new_value
        WHERE age >= 18
    };
    is ($q, q{UPDATE `Tcustomer` SET `first_name` = 'John' WHERE `age` >= '18'});
}

{
    my @new_value= (
        sqlExpr{ first_name = ?      },
        sqlExpr{ surname    = 'Doe'  }
    );
    my $q= sql{
        UPDATE customer SET @new_value
        WHERE age >= 18
    };
    is ($q, q{UPDATE `Tcustomer` SET `first_name` = ?, `surname` = 'Doe' WHERE `age` >= '18'});
}

####
# DESCRIPTION
{
    my $q= sql{ SELECT * FROM mydb };
    is($q, q{SELECT * FROM `Tmydb`});
}

####
# Basic Syntax and Usage

{
    my $query= sql{SELECT foo FROM bar};
    is("$query", q{SELECT `foo` FROM `Tbar`});
}

# Duplicate, will be tested later:
#    my $q= s ql{
#        SELECT foo FROM bar
#    };
#    my @q= s ql{
#        SELECT foo FROM bar ;
#        SELECT foz FROM baz
#    };

eval {
    my $query= sql{SELECT foo FROM bar ; SELECT foz FROM baz};
};
like($@, qr/Multiple results cannot be assigned to scalar/);

{
    my $second= (sqlExpr{ 1, 2, 3})[1];
    is($second, q{'2'});
}

{
    my @col= ('x', 'y');
    my @q= map sql{ SELECT .$_ FROM tab }, @col;
    is($q[0], q{SELECT `x` FROM `Ttab`});
    is($q[1], q{SELECT `y` FROM `Ttab`});
}

####
# Tokens

eval {
    my $q= SQL::Yapp::parse('Stmt', q{SELECT a b FROM c});
};
like($@, qr/but found ident/);

{
    my $q= sql{SELECT a AS b FROM c};
    is($q, q{SELECT `a` AS `b` FROM `Tc`});
}

####
# Differences

{
    my $x= "'test";  # most be quoted properly to work!
    my $y= sql{
        SELECT "difficult: $x"
    };
    is($y, "SELECT 'difficult: 'test'"); # we don't use a complicated quoter, so this is bad
}

{
    my $q= sql{SELECT 1_000_000};
    is($q, q{SELECT '1000000'});
}

{
    my $q= sql{SELECT 0b11};
    is($q, q{SELECT '3'});
}

{
    my $q= sql{SELECT a FROM t LIMIT 5, 2};
    is($q, q{SELECT `a` FROM `Tt` LIMIT 2 OFFSET 5});
}

####
# Perl Interpolation

sub get_where_clause()
{
    return sqlExpr{baz = 5};
}

{
    my $q= sql{
        SELECT foo FROM bar WHERE
            { get_where_clause() }
    };
    is($q, q{SELECT `foo` FROM `Tbar` WHERE `baz` = '5'});
}


{
    my $greeting= 'Hello World';
    my $s1= sql{ SELECT {$greeting} };    # general {...} interpolation
    my $s2= sql{ SELECT $greeting   };    # direct $ interpolation
    my $s3= sql{ SELECT "$greeting" };    # direct string interpolation
    is ($s1, q{SELECT 'Hello World'});
    is ($s2, q{SELECT 'Hello World'});
    is ($s3, q{SELECT 'Hello World'});
}

{
    my $x= 'foo';
    my $s1= sql{ SELECT blah.$x };        # unambiguous: $x is a column name
    my $s2= sql{ SELECT $x.blah };        # unambiguous: $x is a table name
    my $s3= sql{ SELECT "$x" };           # unambiguous: "..." is always a string
    my $s4= sql{ SELECT $x };             # ambiguous: could be string or column,
                                          #   => we resolve this as a string.
    my $s5= sql{ SELECT .$x };            # unambiguous: $x is a column name
                                          #   (the dot is special syntax)
    my $s6= sql{ SELECT ."foo$x" };       # unambiguous: "foo$x" is a column name

    is ($s1, q{SELECT `Tblah`.`foo`});
    is ($s2, q{SELECT `Tfoo`.`blah`});
    is ($s3, q{SELECT 'foo'});
    is ($s4, q{SELECT 'foo'});
    is ($s5, q{SELECT `foo`});
    is ($s6, q{SELECT `foofoo`});
}

{
    my $type= 'b';
    my $q= sql{
        SELECT foo FROM bar WHERE
            {$type eq 'a' ?
                sql{foo >= 2}
            :   sql{foo <= 1}
            }
    };
    is($q, q{SELECT `foo` FROM `Tbar` WHERE `foo` <= '1'});
}

{
    my $type= 'a';
    my $q= sql{
        SELECT foo FROM bar WHERE
            {$type eq 'a' ?
                sql{foo >= 2}
            :   sql{foo <= 1}
            }
    };
    is($q, q{SELECT `foo` FROM `Tbar` WHERE `foo` >= '2'});
}

{
    my $type= 'a';
    my $expr1= sqlExpr{ foo >= 2 };
    my $expr2= sqlExpr{ foo <= 1 };
    my $q= sql{
        SELECT foo FROM bar WHERE
            {$type eq 'a' ?
                $expr1
            :   $expr2
            }
   };
   is ($q, q{SELECT `foo` FROM `Tbar` WHERE `foo` >= '2'});
}

{
    my $is_large= 0;
    my $q= sql{
        SELECT foo FROM bar WHERE
        {$is_large ?
            sqlStmt{UPDATE foz SET bar=5 WHERE name=''}
        :   sqlExpr{test > 5}
        }
    };
}

eval {
    my $is_large= 1;
    my $q= sql{
        SELECT foo FROM bar WHERE
        {$is_large ?
            sqlStmt{UPDATE foz SET bar=5 WHERE name=''}
        :   sqlExpr{test > 5}
        }
    };
};
like($@, qr/Expected SELECT/);

{
    my $is_large= 0;
    my $q= sql{
        SELECT foo FROM bar WHERE
        {$is_large ?
            sqlStmt{SELECT foz FROM baz}
        :   sqlExpr{test > 5}
        }
    };

    is($q, q{SELECT `foo` FROM `Tbar` WHERE `test` > '5'});
}

{
    my $is_large= 1;
    my $q= sql{
        SELECT foo FROM bar WHERE
        {$is_large ?
            sqlStmt{SELECT foz FROM baz}
        :   sqlExpr{test > 5}
        }
    };

    is($q, q{SELECT `foo` FROM `Tbar` WHERE (SELECT `foz` FROM `Tbaz`)});
}

{
    my $x= 3;
    my $q= sql{
        SELECT { 1,2,$x}
    };
    is($q, q{SELECT '1', '2', '3'});
}

{
    my @a= (1,2,3);
    my $q= sql{
        SELECT 0 + @a
    };
    is($q, q{SELECT ('0' + '1' + '2' + '3')});
}

{
    my @a= (1,2,3);
    my $q=sql{
        SELECT 0 AND NOT(@a)
    };
    is($q, q{SELECT ('0' AND (NOT '1') AND (NOT '2') AND (NOT '3'))});
}

{
    no warnings;
    my $q= sql{
        SELECT name AS { 'x', 'y', 'z' }
    };
    is($q, q{SELECT `name` AS `z`});
}
use warnings;

eval {
   my @a= (1,2,3);
   my $q= SQL::Yapp::parse('Stmt', q{
       SELECT name FROM customer WHERE @a  # <--- ERROR
   });
};
like($@, qr/Scalar context, embedded Perl must not be syntactic array/);


####
# Statement Interpolation

{
    my $q= sql{
        SELECT foo FROM bar
    };
    my $q2= sql{
        $q
    };
    is($q2, q{SELECT `foo` FROM `Tbar`});
}

{
    my @q= sql{
        SELECT foo FROM bar ;
        SELECT foz FROM baz
    };
    my @q2= sql{
        @q
    };
    my $q2= join("; ", @q2);
    is($q2, q{SELECT `foo` FROM `Tbar`; SELECT `foz` FROM `Tbaz`});
}

####
# Join Interpolation

{
    my $join= sqlJoin{ NATURAL INNER JOIN foo };
    my $q= sql{ SELECT name FROM bar Join $join WHERE x=y };
    is($q, q{SELECT `name` FROM `Tbar` NATURAL JOIN `Tfoo` WHERE `x` = `y`});
}

{
    my @join= (
        sqlJoin{ NATURAL INNER JOIN foo },
        sqlJoin{ LEFT JOIN baz USING (a) }
    );
    my $q= sql{ SELECT name FROM bar Join @join WHERE x=y };
    is($q, q{SELECT `name` FROM `Tbar` NATURAL JOIN `Tfoo` }.
           q{LEFT JOIN `Tbaz` USING (`a`) WHERE `x` = `y`});
}

####
# Expression Interpolation

{
    my $expr= sqlExpr{ age + 5 };
    my $q= sql{
        SELECT $expr FROM customer
    };
    is($q, q{SELECT (`age` + '5') FROM `Tcustomer`});
}

{
    my @a= (1,2,3);
    my @b= ('a', 'b');
    my $q= sqlExpr{CONCAT(@a,@b,'test')};
    is($q, q{CONCAT('1', '2', '3', 'a', 'b', 'test')});
}

{
    my @a= (1,2,3);
    my $q= sqlExpr{5 * @a};
    is($q, q{('5' * '1' * '2' * '3')});
}

{
    my @a= (1,2,3);
    my $q= sqlExpr{{} * @a};
    is($q, q{('1' * '2' * '3')});
}

{
    my %cond= ( a => 1 ); # more than 1 entries: order is non-deterministic
    my $q= sqlStmt{SELECT x FROM y WHERE {} AND %cond};
    is($q, q{SELECT `x` FROM `Ty` WHERE (`a` = '1')});
}

{
    my @col= ( 'name', 'age' );
    my $q= sql{
        SELECT a FROM b WHERE {} AND (.@col IS NOT NULL)
    };
    is($q, q{SELECT `a` FROM `Tb` WHERE ((`name` IS NOT NULL) AND (`age` IS NOT NULL))});
}

eval{
    my @val= (1,2,3);
    my $q= SQL::Yapp::parse('Stmt', q{ SELECT +@val });  # <--- currently an ERROR
};
like($@, qr/Scalar context, embedded Perl must not be syntactic array/);


{
    my @val= (1,2,3);
    my $q= sql{ SELECT {} + @val };
    is($q, q{SELECT ('1' + '2' + '3')});
}

####
# Expression List Interpolation

{
    my $a= [1,2];
    my ($q1,$q2)= sql{
        SELECT 5 IN (@$a) ;
        SELECT 5 IN $a
    };
    is($q1, q{SELECT '5' IN ('1', '2')});
    is($q2, q{SELECT '5' IN ('1', '2')});
}

{
    my @a= ([1,2], [2,3]);
    my $q= sql{
        INSERT INTO t (x,y) VALUES @a
    };
    is($q, q{INSERT INTO `Tt` (`x`, `y`) VALUES ('1', '2'), ('2', '3')});
}

eval {
    my @a= (1,2);
    my $q= SQL::Yapp::parse('Stmt', q{
        SELECT 5 IN \@a   # <--- ERROR: \@a is no Perl interpolation
    });
};
like($@, qr/Unexpected character/);

####
# Expression Interpolation and AS clause

eval {
    my @col= ('x', 'y');
    my $q= SQL::Yapp::parse 'Stmt', q{
        SELECT .@col AS name    # <--- ERROR: @col not allowed with AS
    };
};
like($@, qr/Scalar context, embedded Perl must not be syntactic array/);

{
    my @col= ('x', 'y');
    my $q=sql{
        SELECT .@col      # <--- OK, will become: SELECT `x`, `y`
    };
    is($q, q{SELECT `x`, `y`});
}

####
# Type Interpolation

{
    my $t1= sqlType{ VARCHAR(50) };
    is($t1, q{VARCHAR (50)});

    my $t2= sqlType{ $t1 CHARACTER SET utf8 };
    is($t2, q{VARCHAR (50) CHARACTER SET `utf8`});

    my $t1b= sqlType{ $t2 DROP CHARACTER SET };
    is($t1b, q{VARCHAR (50)});

    my $t3= sqlType{ $t1 (100) };
    is($t3, q{VARCHAR (100)});

    my $t4= sqlType{ $t2 DECIMAL };
    is($t4, q{DECIMAL (50)});

    my $t5= sqlType{ $t4 CHAR };
    is($t5, q{CHARACTER (50)});
}

{
    my @t1= sqlType{ CHAR(50), VARCHAR(60) };
    my @t2a= sqlType{ @t1 (100) };
    my $t2a= join("; ", @t2a);
    is($t2a, q{CHARACTER (100); VARCHAR (100)});

    my @t2b= sqlType{ CHAR(100), VARCHAR(100) };
    my $t2b= join("; ", @t2b);
    is($t2b, q{CHARACTER (100); VARCHAR (100)});
}

####
# Table Interpolation

{
    my @tab= ( 'foo', 'bar' );
    my $q= sql{
        SELECT name, id FROM @tab
    };
    is ($q, q{SELECT `name`, `id` FROM `Tfoo`, `Tbar`});
}

{
    my $tabspec= sqlTable{ cata.schem.tab };
    my $q= sql{
        SELECT name FROM $tabspec
    };
    is ($q, q{SELECT `name` FROM `cata`.`schem`.`Ttab`});
}

SQL::Yapp::column_prefix('C');
SQL::Yapp::schema_prefix('S');
SQL::Yapp::catalog_prefix('K');

{
    my $tabspec= sqlTable{ cata.schem.tab };
    my $q= sql{
        SELECT $tabspec.name FROM $tabspec
    };
    is ($q, q{SELECT `Kcata`.`Sschem`.`Ttab`.`Cname` FROM `Kcata`.`Sschem`.`Ttab`});
}

SQL::Yapp::column_prefix('');
SQL::Yapp::schema_prefix('');
SQL::Yapp::catalog_prefix('');

eval {
    my $tabspec= sqlTable{ cata.schem.tab };
    my $q= sql{
        SELECT name FROM $tabspec.other  # <--- ERROR!
    };
};
like($@, qr/Expected scalar/);

####
# Column Interpolation

{
    my @col= ('name', sqlColumn{age});
    my $q= sql{
        SELECT .@col
    };
    is($q, q{SELECT `name`, `age`});
}

{
    my @col= ('name', sqlColumn{age});
    my $q= sql{
        SELECT Column @col
    };
    is($q, q{SELECT `name`, `age`});
}

eval {
    my @col= ('name', sqlColumn{age});
    my $q= sql{
        SELECT mytable.@col   # <-- none of @col may be sqlColumn
    };
};
like($@, qr/Expected Column, but found/);

{
    my @col= ('name', 'age');
    my $q= sql{
        SELECT mytable.@col   # <-- none of @col may be sqlColumn
    };
    is($q, q{SELECT `Tmytable`.`name`, `Tmytable`.`age`});
}

{
    my %col= ( 'surname' => 1, 'first_name' => 2 );
    my $q= sql{
        SELECT .%col
    };
    ok($q eq q{SELECT `surname`, `first_name`} or
       $q eq q{SELECT `first_name`, `surname`});
}

{
    my %tab= ( 'x' => 1, 'y' => 2 );
    my %col= ( 'a' => 1, 'b' => 2 );
    my $q= sql{
        SELECT %tab.%col  # <--- works, but is usually not useful
    };
    ok($q eq q{SELECT `Tx`.`a`, `Tx`.`b`, `Ty`.`a`, `Ty`.`b`} ||
       $q eq q{SELECT `Ty`.`a`, `Ty`.`b`, `Tx`.`a`, `Tx`.`b`} ||
       $q eq q{SELECT `Tx`.`b`, `Tx`.`a`, `Ty`.`b`, `Ty`.`a`} ||
       $q eq q{SELECT `Ty`.`b`, `Ty`.`a`, `Tx`.`b`, `Tx`.`a`});
}

####
# GROUP BY / ORDER BY Interpolation

{
    my @a= ();
    my $q= sql{
        SELECT foo FROM bar GROUP BY @a;
    };
    is($q, q{SELECT `foo` FROM `Tbar`});
}

{
    my @a= ('x', 'y');
    my $q= sql{
        SELECT foo FROM bar GROUP BY @a DESC;
    };
    is($q, q{SELECT `foo` FROM `Tbar` GROUP BY `x` DESC, `y` DESC});
}

{
    my $a= 'a';
    is(sqlOrder{ $a },   q{`a`});  # $a is a column name
    is(sqlOrder{ .$a },  q{`a`});  # $a is a column name
    is(sqlOrder{ "$a" }, q{'a'});  # $a is a string
    is(sqlExpr{ $a },    q{'a'});  # $a is a string
    is(sqlExpr{ .$a },   q{`a`});  # $a is a column name
    is(sqlExpr{ "$a" },  q{'a'});  # $a is a string
}

{
    my %a= ( a => 1, b => 1 );
    my $q= sql{
        SELECT a, b FROM t ORDER BY %a
    };
    ok($q eq q{SELECT `a`, `b` FROM `Tt` ORDER BY `a`, `b`} ||
       $q eq q{SELECT `a`, `b` FROM `Tt` ORDER BY `b`, `a`});
}

{
    my %a= ( a => 1, b => 1 );
    my $q= sql{
        SELECT a, b, c FROM t GROUP BY %a
    };
    ok($q eq q{SELECT `a`, `b`, `c` FROM `Tt` GROUP BY `a`, `b`} ||
       $q eq q{SELECT `a`, `b`, `c` FROM `Tt` GROUP BY `b`, `a`});
}

####
# Interpolation In ASC/DESC Clause

{
    my @col= ('x', 'y');
    my $q1=sql{
        SELECT @col FROM t ORDER BY @col DESC
    };
    my $q2=sql{
        SELECT @col FROM t ORDER BY x DESC, y DESC
    };
    is($q1, $q2);
}

{
    my @order= sqlOrder{ a DESC, b ASC };
    my $q1= sql{
        SELECT a, b FROM t GROUP BY @order ORDER BY @order DESC
    };
    is($q1, q{SELECT `a`, `b` FROM `Tt` GROUP BY `a` DESC, `b` ORDER BY `a`, `b` DESC});
}

####
# LIMIT Interpolation

{
    my $q= sql{ SELECT x FROM t LIMIT 10, {undef} };
    is($q, q{SELECT `x` FROM `Tt` LIMIT 18446744073709551615 OFFSET 10});
}

####
# Identifier Name Translation

{
    my $q= sql{
        SELECT c.name FROM customer AS c
    };
    is($q, q{SELECT `Tc`.`name` FROM `Tcustomer` AS `Tc`});
}

####
# DELETE Normalisation

{
    my $q= sql{ DELETE FROM t1, t2 USING t1 CROSS JOIN t2 CROSS JOIN t3
                WHERE (t1.id=t2.id) AND (t2.id=t3.id) };
    is($q, q{DELETE FROM `Tt1`, `Tt2` USING `Tt1` CROSS JOIN `Tt2` CROSS JOIN `Tt3` }.
           q{WHERE ((`Tt1`.`id` = `Tt2`.`id`) AND (`Tt2`.`id` = `Tt3`.`id`))});
}

####
# CASE Normalisation

{
    my @e= sqlExpr{
        CASE a WHEN 1 THEN 0 ELSE 5 END,
        CASE a WHEN 1 THEN 0 END,
        CASE a ELSE 5 END,
        CASE a END
    };
    is($e[0], q{CASE `a` WHEN '1' THEN '0' ELSE '5' END});
    is($e[1], q{CASE `a` WHEN '1' THEN '0' ELSE NULL END});
    is($e[2], q{'5'});
    is($e[3], q{NULL});
}

####
# INSERT ... SET Normalisation

{
    my %a= ( a => 5, b => 6 );
    my $q= sql{
        INSERT INTO t SET %a
    };
    ok($q eq q{INSERT INTO `Tt` (`a`,`b`) VALUES ('5','6')} ||
       $q eq q{INSERT INTO `Tt` (`b`,`a`) VALUES ('6','5')});
}

{
    my %a= ( a => 5, b => 6 );
    my @q= sql{
        INSERT INTO t SET a = 5, b = 6 ;
        INSERT INTO t SET %{{ a => 5, b => 6 }} ;
        INSERT INTO t SET %a, c = 7
    };
    ok($q[0] eq q{INSERT INTO `Tt` (`a`,`b`) VALUES ('5','6')} ||
       $q[0] eq q{INSERT INTO `Tt` (`b`,`a`) VALUES ('6','5')});

    ok($q[1] eq q{INSERT INTO `Tt` (`a`,`b`) VALUES ('5','6')} ||
       $q[1] eq q{INSERT INTO `Tt` (`b`,`a`) VALUES ('6','5')});

    ok($q[2] eq q{INSERT INTO `Tt` (`a`,`b`,`c`) VALUES ('5','6','7')} ||
       $q[2] eq q{INSERT INTO `Tt` (`b`,`a`,`c`) VALUES ('6','5','7')} ||
       $q[2] eq q{INSERT INTO `Tt` (`a`,`c`,`b`) VALUES ('5','7','6')} ||
       $q[2] eq q{INSERT INTO `Tt` (`b`,`c`,`a`) VALUES ('6','7','5')} ||
       $q[2] eq q{INSERT INTO `Tt` (`c`,`a`,`b`) VALUES ('7','5','6')} ||
       $q[2] eq q{INSERT INTO `Tt` (`c`,`b`,`a`) VALUES ('7','6','5')});
}

{
    my $cola=  sqlColumn{ a };
    my $colc=  sqlColumn{ c };
    my $exprb= sqlExpr{ b = 6 };
    my $exprc= sqlExpr{ $colc = 7 };
    my $q= sql{
        INSERT INTO t SET $cola = 5, $exprb, $exprc;
    };
    ok($q eq q{INSERT INTO `Tt` (`a`,`b`,`c`) VALUES ('5','6','7')} ||
       $q eq q{INSERT INTO `Tt` (`b`,`a`,`c`) VALUES ('6','5','7')} ||
       $q eq q{INSERT INTO `Tt` (`a`,`c`,`b`) VALUES ('5','7','6')} ||
       $q eq q{INSERT INTO `Tt` (`b`,`c`,`a`) VALUES ('6','7','5')} ||
       $q eq q{INSERT INTO `Tt` (`c`,`a`,`b`) VALUES ('7','5','6')} ||
       $q eq q{INSERT INTO `Tt` (`c`,`b`,`a`) VALUES ('7','6','5')});
}

{
    is(sqlExpr{a ** b},   q{POWER(`a`, `b`)});
    is(sqlExpr{POW(a,b)}, q{POWER(`a`, `b`)});
}

####
# Manual Parsing

{
    my $perl= SQL::Yapp::parse('ColumnSpec', 'VARCHAR(50) NOT NULL');
    my $q1= eval($perl);
    my $q2= sqlColumnSpec{VARCHAR(50) NOT NULL};
    is($q1, $q2);
}

####
# List of SQL Structures

{
    my $test= sqlExpr{a == 5};
    my $q= sql{SELECT a FROM t WHERE $test};
    is($q, q{SELECT `a` FROM `Tt` WHERE `a` = '5'});
}

0;
