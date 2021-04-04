package jp.co.future.uroborosql.parser;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.Emp;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContextProviderImpl;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.exception.EndCommentNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.TokenNotClosedRuntimeException;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.SqlNode;
import jp.co.future.uroborosql.utils.StringFunction;

public class SqlParserTest {
	private SqlConfig sqlConfig;

	@BeforeEach
	public void setUp() throws Exception {
		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setExecutionContextProvider(new ExecutionContextProviderImpl()
						.setEnumConstantPackageNames(Arrays.asList(TestEnum1.class.getPackage().getName())))
				.build();
	}

	private void sqlAssertion(final String original, final String expected) {
		SqlParser parser = new SqlParserImpl(original, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();

		var transformer = parser.parse();
		transformer.transform(ctx);
		var transformed = ctx.getExecutableSql().trim().replaceAll("\\s+", " ");
		assertThat(transformed, is(expected));
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElif() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF FALSE ELSE
	@Test
	public void testElif2() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElif3() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4 --ELSE 3=3/*END*/";
		var sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELSE
	@Test
	public void testElif4() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	public void testElif4_1() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE)
	@Test
	public void testElif5() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELSE (TRUE)
	@Test
	public void testElif6() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	@Test
	public void testElif7() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4/*END*/";
		var sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE
	@Test
	public void testElif8() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2/*END*/";
		var sql2 = "";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE)
	//  IF (TRUE)
	//  ELIF FALSE
	//  ELIF FALSE
	// ELSE
	@Test
	public void testElif_nest1() throws Exception {
		var sql = "/*IF true*/1=1 /*IF true*/and 11=11 /*ELIF false*/and 22=22/*END*/ /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "1=1 and 11=11";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE)
	// IF FALSE
	// ELIF (TRUE)
	// ELIF FALSE
	// ELSE
	@Test
	public void testElif_nest2() throws Exception {
		var sql = "/*IF true*/1=1 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*/ /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "1=1 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF (TRUE)
	// IF (TRUE)
	// ELIF FALSE
	// ELSE
	@Test
	public void testElif_nest3() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*IF true*/and 11=11 /*ELIF false*/and 22=22/*END*/ --ELSE 3=3/*END*/";
		var sql2 = "2=2 and 11=11";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF (TRUE)
	// IF FALSE
	// ELIF (TRUE)
	// ELSE
	@Test
	public void testElif_nest4() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*/ --ELSE 3=3/*END*/";
		var sql2 = "2=2 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF FALSE
	// ELSE
	// IF (TRUE)
	// ELIF FALSE
	@Test
	public void testElif_nest5() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3 /*IF true*/and 11=11 /*ELIF false*/and 22=22/*END*//*END*/";
		var sql2 = "3=3 and 11=11";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF FALSE
	// ELSE
	// IF FALSE
	// ELIF (TRUE)
	@Test
	public void testElif_nest6() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2  --ELSE 3=3 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*//*END*/";
		var sql2 = "3=3 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE END
	@Test
	public void testBeginElif() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 /*END*//*END*/";
		var sql2 = "";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF (TRUE) ELIF FALSE END
	@Test
	public void testBeginElif2() throws Exception {
		var sql = "/*BEGIN*//*IF true*/1=1 /*ELIF false*/2=2 /*END*//*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF (TRUE) END
	@Test
	public void testBeginElif3() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF true*/2=2 /*END*//*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE ELSE END
	@Test
	public void testBeginElif4() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3/*END*//*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE ELSE END
	@Test
	public void testBeginElif5() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 /*ELSE*/ 3=3/*END*//*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElifWithNewElse() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF FALSE ELSE
	@Test
	public void testElifWithNewElse2() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElifWithNewElse3() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4 /*ELSE*/ 3=3/*END*/";
		var sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELSE
	@Test
	public void testElifWithNewElse4() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	public void testElifWithNewElse4_1() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	public void testElifWithNewElse4_2() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF -- COMMENT ELSE 2
	@Test
	public void testIFELSECOMMENT_2() throws Exception {
		var sql = "/*IF false*/1=1 -- comment /*ELIF true*/2=2 /*ELSE*/3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	@Test
	public void testParse() throws Exception {
		var sql = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql));
	}

	@Test
	public void testParseNormalComment() throws Exception {
		var sql = "SELECT /* empの全件検索 */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql));
	}

	@Test
	public void testParseLineComment() throws Exception {
		var sql = "SELECT -- empの全件検索  * FROM emp WHERE job = /*job*/'CLERK'";
		var sql2 = "SELECT -- empの全件検索  * FROM emp WHERE job = ?/*job*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("job", "CLERK");
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql2));
	}

	@Test
	public void testParseHintComment() throws Exception {
		var sql = "SELECT /*+ FIRST_ROWS */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql));
	}

	@Test
	public void testParseEndSemicolon() throws Exception {
		testParseEndSemicolon(";");
		testParseEndSemicolon(";\t");
		testParseEndSemicolon("; ");
	}

	private void testParseEndSemicolon(final String endChar) {
		var sql = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql + endChar, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql));
	}

	@Test
	public void testCommentEndNotFound() throws Exception {
		var sql = "SELECT * FROM emp/*hoge";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		try {
			parser.parse();
			assertThat("1", false);
		} catch (TokenNotClosedRuntimeException ex) {
			System.out.println(ex);
		}
	}

	@Test
	public void testParseBindVariable() throws Exception {
		var sql = "SELECT * FROM emp WHERE job = /*job*/'CLERK' AND deptno = /*deptno*/20";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*job*/ AND deptno = ?/*deptno*/";
		var sql3 = "SELECT * FROM emp WHERE job = ";
		var sql4 = " AND deptno = ";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var job = "CLERK";
		var deptno = Integer.valueOf(20);
		ctx.param("job", job);
		ctx.param("deptno", deptno);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));

		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(job));
		assertThat(vars[1], is(deptno));
		assertThat(root.getChildSize(), is(4));
		var sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		var varNode = (BindVariableNode) root.getChild(1);
		assertThat(varNode.getExpression(), is("job"));
		var sqlNode2 = (SqlNode) root.getChild(2);
		assertThat(sqlNode2.getSql(), is(sql4));
		var varNode2 = (BindVariableNode) root.getChild(3);
		assertThat(varNode2.getExpression(), is("deptno"));
	}

	@Test
	public void testParseBindVariable2() throws Exception {
		var sql = "SELECT * FROM emp WHERE job = /* job*/'CLERK'"; // コメントの先頭が空白ならコメントとして扱う
		var sql2 = "SELECT * FROM emp WHERE job = /* job*/'CLERK'";
		var sql3 = "SELECT * FROM emp WHERE job = ";
		var sql4 = "/* job*/";
		var sql5 = "'CLERK'";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		assertThat(root.getChildSize(), is(3));
		var sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		var sqlNode2 = (SqlNode) root.getChild(1);
		assertThat(sqlNode2.getSql(), is(sql4));
		var sqlNode3 = (SqlNode) root.getChild(2);
		assertThat(sqlNode3.getSql(), is(sql5));
	}

	@Test
	public void testParseWhiteSpace() throws Exception {
		var sql = "SELECT * FROM emp WHERE empno = /*empno*/1 AND 1 = 1";
		var sql2 = "SELECT * FROM emp WHERE empno = ?/*empno*/ AND 1 = 1";
		var sql3 = " AND 1 = 1";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var empno = Integer.valueOf(7788);
		ctx.param("empno", empno);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var sqlNode = (SqlNode) root.getChild(2);
		assertThat(sqlNode.getSql(), is(sql3));
	}

	@Test
	public void testParseIf() throws Exception {
		var sql = "SELECT * FROM emp/*IF job != null*/ WHERE job = /*job*/'CLERK'/*END*/";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*job*/";
		var sql3 = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var job = "CLERK";
		ctx.param("job", job);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(job));
		assertThat(root.getChildSize(), is(2));
		var sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		var ifNode = (IfNode) root.getChild(1);
		assertThat(ifNode.getExpression(), is("job != null"));
		assertThat(ifNode.getChildSize(), is(2));
		var sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertThat(sqlNode2.getSql(), is(" WHERE job = "));
		var varNode = (BindVariableNode) ifNode.getChild(1);
		assertThat(varNode.getExpression(), is("job"));
		var ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertThat(ctx2.getExecutableSql(), is(sql3));
	}

	@Test
	public void testParseIf2() throws Exception {
		var sql = "/*IF aaa != null*/aaa/*IF bbb != null*/bbb/*END*//*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is(""));
		ctx.param("aaa", null);
		ctx.param("bbb", "hoge");
		root.accept(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is(""));
		ctx.param("aaa", "hoge");
		root.accept(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is("aaabbb"));
		var ctx2 = sqlConfig.context();
		ctx2.param("aaa", "hoge");
		ctx2.param("bbb", null);
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertThat(ctx2.getExecutableSql(), is("aaa"));
	}

	@Test
	public void testParseIf3() throws Exception {
		var sql = "SELECT * FROM emp/*IF emp != null && emp.job != null*/ WHERE job = /*emp.job*/'CLERK'/*END*/";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*emp.job*/";
		var sql3 = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var emp = new Emp();
		emp.setJob("CLERK");
		ctx.param("emp", emp);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(emp.getJob()));
		assertThat(root.getChildSize(), is(2));
		var sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		var ifNode = (IfNode) root.getChild(1);
		assertThat(ifNode.getExpression(), is("emp != null && emp.job != null"));
		assertThat(ifNode.getChildSize(), is(2));
		var sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertThat(sqlNode2.getSql(), is(" WHERE job = "));
		var varNode = (BindVariableNode) ifNode.getChild(1);
		assertThat(varNode.getExpression(), is("emp.job"));
		var ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertThat(ctx2.getExecutableSql(), is(sql3));
	}

	@Test
	public void testParseElse() throws Exception {
		var sql = "SELECT * FROM emp WHERE /*IF job != null*/job = /*job*/'CLERK'-- ELSE job is null/*END*/";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*job*/";
		var sql3 = "SELECT * FROM emp WHERE job is null";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var job = "CLERK";
		ctx.param("job", job);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is(sql2));

		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(job));
		var ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertThat(ctx2.getExecutableSql(), is(sql3));
	}

	@Test
	public void testParseElse2() throws Exception {
		var sql = "/*IF false*/aaa--ELSE bbb = /*bbb*/123/*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var bbb = Integer.valueOf(123);
		ctx.param("bbb", bbb);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is("bbb = ?/*bbb*/"));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(bbb));
	}

	@Test
	public void testParseElse3() throws Exception {
		var sql = "/*IF false*/aaa--ELSE bbb/*IF false*/ccc--ELSE ddd/*END*//*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is("bbbddd"));
	}

	@Test
	public void testElse4() throws Exception {
		var sql = "SELECT * FROM emp/*BEGIN*/ WHERE /*IF false*/aaa-- ELSE AND deptno = 10/*END*//*END*/";
		var sql2 = "SELECT * FROM emp WHERE deptno = 10";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
	}

	@Test
	public void testBegin() throws Exception {
		var sql = "SELECT * FROM emp/*BEGIN*/ WHERE /*IF job != null*/job = /*job*/'CLERK'/*END*//*IF deptno != null*/ AND deptno = /*deptno*/20/*END*//*END*/";
		var sql2 = "SELECT * FROM emp";
		var sql3 = "SELECT * FROM emp WHERE job = ?/*job*/";
		var sql4 = "SELECT * FROM emp WHERE job = ?/*job*/ AND deptno = ?/*deptno*/";
		var sql5 = "SELECT * FROM emp WHERE  deptno = ?/*deptno*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);

		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var bNames = ctx.getBindNames();
		assertThat(bNames.size(), is(0));

		var ctx2 = sqlConfig.context();
		ctx2.param("job", "CLERK");
		ctx2.param("deptno", null);
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertThat(ctx2.getExecutableSql(), is(sql3));
		var bNames2 = ctx2.getBindNames();
		assertThat(bNames2.size(), is(1));

		var ctx3 = sqlConfig.context();
		ctx3.param("job", "CLERK");
		ctx3.param("deptno", Integer.valueOf(20));
		root.accept(ctx3);
		System.out.println(ctx3.getExecutableSql());
		assertThat(ctx3.getExecutableSql(), is(sql4));
		var bNames3 = ctx3.getBindNames();
		assertThat(bNames3.size(), is(2));

		var ctx4 = sqlConfig.context();
		ctx4.param("deptno", Integer.valueOf(20));
		ctx4.param("job", null);
		root.accept(ctx4);
		System.out.println(ctx4.getExecutableSql());
		assertThat(ctx4.getExecutableSql(), is(sql5));
		var bNames4 = ctx4.getBindNames();
		assertThat(bNames4.size(), is(1));
	}

	@Test
	public void testBeginAnd() throws Exception {
		var sql = "/*BEGIN*/WHERE /*IF true*/aaa BETWEEN /*bbb*/111 AND /*ccc*/123/*END*//*END*/";
		var sql2 = "WHERE aaa BETWEEN ?/*bbb*/ AND ?/*ccc*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("bbb", "111");
		ctx.param("ccc", "222");
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertThat(ctx.getExecutableSql(), is(sql2));
		var bNames = ctx.getBindNames();
		assertThat(bNames.size(), is(2));
		assertThat(bNames.get(0), is("bbb"));
		assertThat(bNames.get(1), is("ccc"));
	}

	@Test
	public void testIn() throws Exception {
		var sql = "SELECT * FROM emp WHERE deptno IN /*deptnoList*/(10, 20) ORDER BY ename";
		var sql2 = "SELECT * FROM emp WHERE deptno IN (?, ?)/*deptnoList*/ ORDER BY ename";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		List<Integer> deptnoList = new ArrayList<>();
		deptnoList.add(Integer.valueOf(10));
		deptnoList.add(Integer.valueOf(20));
		ctx.param("deptnoList", deptnoList);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(Integer.valueOf(10)));
		assertThat(vars[1], is(Integer.valueOf(20)));
	}

	@Test
	public void testIn2() throws Exception {
		var sql = "SELECT * FROM emp WHERE deptno IN /*deptnoList*/(10, 20) ORDER BY ename";
		var sql2 = "SELECT * FROM emp WHERE deptno IN (?, ?)/*deptnoList*/ ORDER BY ename";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		int[] deptnoArray = { 10, 20 };
		ctx.param("deptnoList", deptnoArray);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(Integer.valueOf(10)));
		assertThat(vars[1], is(Integer.valueOf(20)));
	}

	@Test
	public void testIn3() throws Exception {
		var sql = "SELECT * FROM emp WHERE ename IN /*enames*/('SCOTT','MARY') AND job IN /*jobs*/('ANALYST', 'FREE')";
		var sql2 = "SELECT * FROM emp WHERE ename IN (?, ?)/*enames*/ AND job IN (?, ?)/*jobs*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		String[] enames = { "SCOTT", "MARY" };
		String[] jobs = { "ANALYST", "FREE" };
		ctx.param("enames", enames);
		ctx.param("jobs", jobs);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(4));
		assertThat(vars[0], is("SCOTT"));
		assertThat(vars[1], is("MARY"));
		assertThat(vars[2], is("ANALYST"));
		assertThat(vars[3], is("FREE"));
	}

	@Test
	public void testParseBindVariable3() throws Exception {
		var sql = "BETWEEN sal ? AND ?";
		var sql2 = "BETWEEN sal ?/*$1*/ AND ?/*$2*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("$1", Integer.valueOf(0));
		ctx.param("$2", Integer.valueOf(1000));
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(Integer.valueOf(0)));
		assertThat(vars[1], is(Integer.valueOf(1000)));
	}

	@Test
	public void testParseBindVariable4() throws Exception {
		var sql = "SELECT * FROM emp WHERE job = /*emp.job*/'CLERK' AND deptno = /*emp.deptno*/20";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*emp.job*/ AND deptno = ?/*emp.deptno*/";
		var sql3 = "SELECT * FROM emp WHERE job = ";
		var sql4 = " AND deptno = ";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var emp = new Emp();
		emp.setJob("CLERK");
		emp.setDeptno(Integer.valueOf(20));
		ctx.param("emp", emp);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));

		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(emp.getJob()));
		assertThat(vars[1], is(emp.getDeptno()));
		assertThat(root.getChildSize(), is(4));
		var sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		var varNode = (BindVariableNode) root.getChild(1);
		assertThat(varNode.getExpression(), is("emp.job"));
		var sqlNode2 = (SqlNode) root.getChild(2);
		assertThat(sqlNode2.getSql(), is(sql4));
		var varNode2 = (BindVariableNode) root.getChild(3);
		assertThat(varNode2.getExpression(), is("emp.deptno"));
	}

	@Test
	public void testParseBindVariable5() throws Exception {
		var sql = "/*(count + 1)*/";
		var sql2 = "?/*(count + 1)*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("count", 3);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(4));
	}

	@Test
	public void testParseBindVariable6() throws Exception {
		var sql = "SELECT * FROM EMP WHERE ENAME IN /*SF.split(enames, ',')*/()";
		var sql2 = "SELECT * FROM EMP WHERE ENAME IN (?, ?)/*SF.split(enames, ',')*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param(StringFunction.SHORT_NAME, new StringFunction(new DefaultDialect()));
		ctx.param("enames", "SCOTT,MARY");
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
		var vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is("SCOTT"));
		assertThat(vars[1], is("MARY"));
	}

	@Test
	public void testEndNotFound() throws Exception {
		var sql = "/*BEGIN*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		try {
			parser.parse();
			assertThat("1", false);
		} catch (EndCommentNotFoundRuntimeException ex) {
			System.out.println(ex);
		}
	}

	@Test
	public void testEndParent() throws Exception {
		var sql = "INSERT INTO ITEM (ID, NUM) VALUES (/*id*/1, /*num*/20)";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("id", Integer.valueOf(0));
		ctx.param("num", Integer.valueOf(1));
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql().endsWith(")"), is(true));
	}

	@Test
	public void testEmbeddedValue() throws Exception {
		var sql = "xx /*#aaa*/ xx";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("aaa", Integer.valueOf(0));
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is("xx '0'/*#aaa*/ xx"));
	}

	@Test
	public void testEmbeddedValue2() throws Exception {
		var sql = "/*$emp.deptno*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var emp = new Emp();
		emp.setDeptno(Integer.valueOf(0));
		ctx.param("emp", emp);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is("0/*$emp.deptno*/"));
	}

	/**
	 * 埋め込み変数のエスケープ処理テスト（SQLインジェクション対策）
	 *
	 * @throws Exception
	 */
	@Test
	public void testEmbeddedValue3() throws Exception {
		var sql = "xx /*#aaa*/ xx";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("aaa", "bb'bb");
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is("xx 'bb''bb'/*#aaa*/ xx"));
	}

	@Test
	public void testStringFunction1() throws Exception {
		var sql = "/*IF SF.isEmpty(val1)*/1=1 /*ELIF SF.containsAny(val2, val3)*/2=2 --ELSE 3=3/*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("val1", null);
		ctx.param("val2", "aaabbbccc");
		ctx.param("val3", "ab");

		var sql2 = "1=1 ";
		ctx.param(StringFunction.SHORT_NAME, new StringFunction(new DefaultDialect()));
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql(), is(sql2));
	}

	@SuppressWarnings("deprecation")
	private void initData(final Connection conn) throws SQLException {
		// テーブル作成
		var stmt = conn.createStatement();
		stmt.execute("create table TEST( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE )");

		var pstmt = conn.prepareStatement("insert into test values (?, ?, ?, ?)");
		pstmt.setInt(1, 1);
		pstmt.setString(2, "aaa");
		pstmt.setInt(3, 10);
		pstmt.setDate(4, new java.sql.Date(100, 0, 1));
		pstmt.addBatch();

		pstmt.setInt(1, 2);
		pstmt.setString(2, "あああ");
		pstmt.setInt(3, 20);
		pstmt.setDate(4, new java.sql.Date(100, 1, 1));
		pstmt.addBatch();

		pstmt.setInt(1, 3);
		pstmt.setString(2, "1111");
		pstmt.setInt(3, 3000);
		pstmt.setDate(4, new java.sql.Date(100, 2, 1));
		pstmt.addBatch();

		pstmt.executeBatch();
		conn.commit();
	}

	@Test
	public void testParamMissMatch1() throws Exception {
		PreparedStatement st = null;

		try (var conn = DriverManager.getConnection("jdbc:h2:mem:testParamMissMatch1")) {
			initData(conn);

			var sql = "select * from test where id = /*val1*/1 and name = /*val2*/'' and age = /*val3*/1";
			SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
					sqlConfig.getDialect().isRemoveTerminator(), true);
			var ctx = sqlConfig.context();
			ctx.param("val1", "1");
			ctx.param("val3", "20");

			var transformer = parser.parse();
			transformer.transform(ctx);

			st = conn.prepareStatement(ctx.getExecutableSql());
			ctx.bindParams(st);
			assertThat("テスト失敗", false);
		} catch (ParameterNotFoundRuntimeException ex) {
			var msg = ex.getMessage();
			assertThat(msg, is("Parameter [val2] is not found."));
		} catch (Exception ex) {
			assertThat("期待しない例外. ex=" + ex.getMessage(), false);
		}
	}

	@Test
	public void testParamMissMatch2() throws Exception {
		PreparedStatement st = null;

		try (var conn = DriverManager.getConnection("jdbc:h2:mem:testParamMissMatch2")) {
			initData(conn);

			var sql = "select * from test where id = /*val1*/1 and name = /*val2*/'' and age = /*val3*/1";
			SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
					sqlConfig.getDialect().isRemoveTerminator(), true);
			var ctx = sqlConfig.context();
			ctx.param("val1", "1");

			var transformer = parser.parse();
			transformer.transform(ctx);

			st = conn.prepareStatement(ctx.getExecutableSql());
			ctx.bindParams(st);
			assertThat("テスト失敗", false);
		} catch (ParameterNotFoundRuntimeException ex) {
			var msg = ex.getMessage();
			assertThat(msg, is("Parameter [val2, val3] is not found."));
		} catch (Exception ex) {
			assertThat("期待しない例外. ex=" + ex.getMessage(), false);
		}
	}

	@Test
	public void testParamMissMatch3() throws Exception {
		PreparedStatement st = null;

		try (var conn = DriverManager.getConnection("jdbc:h2:mem:testParamMissMatch3")) {
			initData(conn);

			var sql = "select * from test where id = /*val1*/1 and name = /*val2*/'' and age = /*val3*/1";
			SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
					sqlConfig.getDialect().isRemoveTerminator(), true);
			var ctx = sqlConfig.context();
			ctx.param("val1", "1");
			ctx.param("val2", "aa");
			ctx.param("val3", "20");

			var transformer = parser.parse();
			transformer.transform(ctx);

			st = conn.prepareStatement(ctx.getExecutableSql());
			ctx.bindParams(st);
		} catch (Exception ex) {
			assertThat("期待しない例外. ex=" + ex.getMessage(), false);
		}
	}

	@Test
	public void testELSECOMMENT1() throws Exception {
		var sql = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT1.sql")),
				StandardCharsets.UTF_8);
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("param1", "1");

		var transformer = parser.parse();
		transformer.transform(ctx);
		var sql2 = new String(
				Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT1.sql_expected")),
				StandardCharsets.UTF_8);
		var sql3 = ctx.getExecutableSql();
		System.out.println(sql3);
		assertThat(sql3, is(sql2));
	}

	@Test
	public void testELSECOMMENT2() throws Exception {
		var sql = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT2.sql")),
				StandardCharsets.UTF_8);
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("param1", "2");

		var transformer = parser.parse();
		transformer.transform(ctx);
		var sql2 = new String(
				Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT2.sql_expected")),
				StandardCharsets.UTF_8);
		var sql3 = ctx.getExecutableSql();
		System.out.println(sql3);
		assertThat(sql3, is(sql2));
	}

	@Test
	public void testParseNodeDecrare() throws Exception {
		var sql = "DECLARE /* _SQL_IDENTIFIER_ */	/*IF projectStage != \"dev\"*/	PRAGMA AUTONOMOUS_TRANSACTION;	/*END*/BEGIN	SELECT 'aaa' as AAA FROM DUAL;END;";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("projectStage", "ci");

		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.format("%s\r\n", ctx.getExecutableSql());
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testParseNode() throws Exception {
		var sql = "aaa /*IF purCd != null*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is("aaa  bbb"));
	}

	// IF ENUM EQ
	@Test
	public void testParseEnumEq1() throws Exception {
		var sql = "aaa /*IF CLS_TEST_ENUM1_A eq bindEnum*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("bindEnum", TestEnum1.A);

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is("aaa 1=1 bbb"));
	}

	// IF ENUM ==
	@Test
	public void testParseEnumEq2() throws Exception {
		var sql = "aaa /*IF CLS_TEST_ENUM1_A == bindEnum*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("bindEnum", TestEnum1.A);

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is("aaa 1=1 bbb"));
	}

	// Delete AND / OR immediately after WHERE clause
	@Test
	public void testDeleteImmediatelyAfterWhereClause() throws Exception {
		// OK Case
		var sql = "aaa Where /*IF true*/And col1 = 1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is("aaa Where col1 = 1 bbb"));

		// OK Case in \r\n
		sql = "aaa Where\r\n/*IF true*/or\r\ncol1 = 1/*END*/ bbb";

		parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(), sqlConfig.getDialect().isRemoveTerminator(),
				true);
		ctx = sqlConfig.context();

		transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is("aaa Where\r\ncol1 = 1 bbb"));

		// NG Case
		sql = "aaa WHERE /*IF true*/ORDER = 1/*END*/ bbb";

		parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(), sqlConfig.getDialect().isRemoveTerminator(),
				true);
		ctx = sqlConfig.context();

		transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is("aaa WHERE ORDER = 1 bbb"));
	}

}