package jp.co.future.uroborosql.parser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
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
import jp.co.future.uroborosql.utils.SqlFunction;

public class SqlParserTest {
	private SqlConfig sqlConfig;

	@BeforeEach
	public void setUp() throws Exception {
		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setExecutionContextProvider(new ExecutionContextProviderImpl()
						.setEnumConstantPackageNames(List.of(TestEnum1.class.getPackage().getName())))
				.build();
	}

	private void sqlAssertion(final String original, final String expected) {
		SqlParser parser = new SqlParserImpl(original, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();

		var transformer = parser.parse();
		transformer.transform(ctx);
		var transformed = ctx.getExecutableSql().trim().replaceAll("\\s+", " ");
		assertEquals(expected, transformed, "結果が一致しません。");
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	void testElif() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF FALSE ELSE
	@Test
	void testElif2() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELIF (TRUE) ELSE
	@Test
	void testElif3() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4 --ELSE 3=3/*END*/";
		var sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELSE
	@Test
	void testElif4() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	void testElif4_1() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE)
	@Test
	void testElif5() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELSE (TRUE)
	@Test
	void testElif6() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	@Test
	void testElif7() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4/*END*/";
		var sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE
	@Test
	void testElif8() throws Exception {
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
	void testElif_nest1() throws Exception {
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
	void testElif_nest2() throws Exception {
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
	void testElif_nest3() throws Exception {
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
	void testElif_nest4() throws Exception {
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
	void testElif_nest5() throws Exception {
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
	void testElif_nest6() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2  --ELSE 3=3 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*//*END*/";
		var sql2 = "3=3 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE END
	@Test
	void testBeginElif() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 /*END*//*END*/";
		var sql2 = "";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF (TRUE) ELIF FALSE END
	@Test
	void testBeginElif2() throws Exception {
		var sql = "/*BEGIN*//*IF true*/1=1 /*ELIF false*/2=2 /*END*//*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF (TRUE) END
	@Test
	void testBeginElif3() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF true*/2=2 /*END*//*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE ELSE END
	@Test
	void testBeginElif4() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3/*END*//*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE ELSE END
	@Test
	void testBeginElif5() throws Exception {
		var sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 /*ELSE*/ 3=3/*END*//*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	void testElifWithNewElse() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF FALSE ELSE
	@Test
	void testElifWithNewElse2() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELIF (TRUE) ELSE
	@Test
	void testElifWithNewElse3() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4 /*ELSE*/ 3=3/*END*/";
		var sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELSE
	@Test
	void testElifWithNewElse4() throws Exception {
		var sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	void testElifWithNewElse4_1() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	void testElifWithNewElse4_2() throws Exception {
		var sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/3=3/*END*/";
		var sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF -- COMMENT ELSE 2
	@Test
	void testIFELSECOMMENT_2() throws Exception {
		var sql = "/*IF false*/1=1 -- comment /*ELIF true*/2=2 /*ELSE*/3=3/*END*/";
		var sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	@Test
	void testParse() throws Exception {
		var sql = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql(), "1");
	}

	@Test
	void testParseNormalComment() throws Exception {
		var sql = "SELECT /* empの全件検索 */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql(), "1");
	}

	@Test
	void testParseLineComment() throws Exception {
		var sql = "SELECT -- empの全件検索  * FROM emp WHERE job = /*job*/'CLERK'";
		var sql2 = "SELECT -- empの全件検索  * FROM emp WHERE job = ?/*job*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("job", "CLERK");
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql2, ctx.getExecutableSql(), "1");
	}

	@Test
	void testParseHintComment() throws Exception {
		var sql = "SELECT /*+ FIRST_ROWS */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql(), "1");
	}

	@Test
	void testParseEndSemicolon() throws Exception {
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
		assertEquals(sql, ctx.getExecutableSql(), "1");
	}

	@Test
	void testCommentEndNotFound() throws Exception {
		var sql = "SELECT * FROM emp/*hoge";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		try {
			parser.parse();
			fail("1");
		} catch (TokenNotClosedRuntimeException ex) {
			System.out.println(ex);
		}
	}

	@Test
	void testParseBindVariable() throws Exception {
		var sql = "SELECT * FROM emp WHERE job = /*job*/'CLERK' AND deptno = /*deptno*/20";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*job*/ AND deptno = ?/*deptno*/";
		var sql3 = "SELECT * FROM emp WHERE job = ";
		var sql4 = " AND deptno = ";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var job = "CLERK";
		var deptno = 20;
		ctx.param("job", job);
		ctx.param("deptno", deptno);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");

		var vars = ctx.getBindVariables();
		assertEquals(2, vars.length, "2");
		assertEquals(job, vars[0], "3");
		assertEquals(deptno, vars[1], "4");
		assertEquals(4, root.getChildSize(), "5");
		var sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql(), "6");
		var varNode = (BindVariableNode) root.getChild(1);
		assertEquals("job", varNode.getExpression(), "7");
		var sqlNode2 = (SqlNode) root.getChild(2);
		assertEquals(sql4, sqlNode2.getSql(), "8");
		var varNode2 = (BindVariableNode) root.getChild(3);
		assertEquals("deptno", varNode2.getExpression(), "9");
	}

	@Test
	void testParseBindVariable2() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		assertEquals(3, root.getChildSize(), "2");
		var sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql(), "3");
		var sqlNode2 = (SqlNode) root.getChild(1);
		assertEquals(sql4, sqlNode2.getSql(), "4");
		var sqlNode3 = (SqlNode) root.getChild(2);
		assertEquals(sql5, sqlNode3.getSql(), "5");
	}

	@Test
	void testParseWhiteSpace() throws Exception {
		var sql = "SELECT * FROM emp WHERE empno = /*empno*/1 AND 1 = 1";
		var sql2 = "SELECT * FROM emp WHERE empno = ?/*empno*/ AND 1 = 1";
		var sql3 = " AND 1 = 1";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var empno = 7788;
		ctx.param("empno", empno);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var sqlNode = (SqlNode) root.getChild(2);
		assertEquals(sql3, sqlNode.getSql(), "2");
	}

	@Test
	void testParseIf() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(1, vars.length, "2");
		assertEquals(job, vars[0], "3");
		assertEquals(2, root.getChildSize(), "4");
		var sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql(), "5");
		var ifNode = (IfNode) root.getChild(1);
		assertEquals("job != null", ifNode.getExpression(), "6");
		assertEquals(2, ifNode.getChildSize(), "7");
		var sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertEquals(" WHERE job = ", sqlNode2.getSql(), "8");
		var varNode = (BindVariableNode) ifNode.getChild(1);
		assertEquals("job", varNode.getExpression(), "9");
		var ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertEquals(sql3, ctx2.getExecutableSql(), "10");
	}

	@Test
	void testParseIf2() throws Exception {
		var sql = "/*IF aaa != null*/aaa/*IF bbb != null*/bbb/*END*//*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("", ctx.getExecutableSql(), "1");
		ctx.param("aaa", null);
		ctx.param("bbb", "hoge");
		root.accept(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("", ctx.getExecutableSql(), "2");
		ctx.param("aaa", "hoge");
		root.accept(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("aaabbb", ctx.getExecutableSql(), "3");
		var ctx2 = sqlConfig.context();
		ctx2.param("aaa", "hoge");
		ctx2.param("bbb", null);
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertEquals("aaa", ctx2.getExecutableSql(), "4");
	}

	@Test
	void testParseIf3() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(1, vars.length, "2");
		assertEquals(emp.getJob(), vars[0], "3");
		assertEquals(2, root.getChildSize(), "4");
		var sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql(), "5");
		var ifNode = (IfNode) root.getChild(1);
		assertEquals("emp != null && emp.job != null", ifNode.getExpression(), "6");
		assertEquals(2, ifNode.getChildSize(), "7");
		var sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertEquals(" WHERE job = ", sqlNode2.getSql(), "8");
		var varNode = (BindVariableNode) ifNode.getChild(1);
		assertEquals("emp.job", varNode.getExpression(), "9");
		var ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertEquals(sql3, ctx2.getExecutableSql(), "10");
	}

	@Test
	void testParseElse() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");

		var vars = ctx.getBindVariables();
		assertEquals(1, vars.length, "2");
		assertEquals(job, vars[0], "3");
		var ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertEquals(sql3, ctx2.getExecutableSql(), "4");
	}

	@Test
	void testParseElse2() throws Exception {
		var sql = "/*IF false*/aaa--ELSE bbb = /*bbb*/123/*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var bbb = 123;
		ctx.param("bbb", bbb);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("bbb = ?/*bbb*/", ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(1, vars.length, "2");
		assertEquals(bbb, vars[0], "3");
	}

	@Test
	void testParseElse3() throws Exception {
		var sql = "/*IF false*/aaa--ELSE bbb/*IF false*/ccc--ELSE ddd/*END*//*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("bbbddd", ctx.getExecutableSql(), "1");
	}

	@Test
	void testElse4() throws Exception {
		var sql = "SELECT * FROM emp/*BEGIN*/ WHERE /*IF false*/aaa-- ELSE AND deptno = 10/*END*//*END*/";
		var sql2 = "SELECT * FROM emp WHERE deptno = 10";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");
	}

	@Test
	void testBegin() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var bNames = ctx.getBindNames();
		assertEquals(0, bNames.size());

		var ctx2 = sqlConfig.context();
		ctx2.param("job", "CLERK");
		ctx2.param("deptno", null);
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertEquals(sql3, ctx2.getExecutableSql(), "2");
		var bNames2 = ctx2.getBindNames();
		assertEquals(1, bNames2.size());

		var ctx3 = sqlConfig.context();
		ctx3.param("job", "CLERK");
		ctx3.param("deptno", 20);
		root.accept(ctx3);
		System.out.println(ctx3.getExecutableSql());
		assertEquals(sql4, ctx3.getExecutableSql(), "3");
		var bNames3 = ctx3.getBindNames();
		assertEquals(2, bNames3.size());

		var ctx4 = sqlConfig.context();
		ctx4.param("deptno", 20);
		ctx4.param("job", null);
		root.accept(ctx4);
		System.out.println(ctx4.getExecutableSql());
		assertEquals(sql5, ctx4.getExecutableSql(), "4");
		var bNames4 = ctx4.getBindNames();
		assertEquals(1, bNames4.size());
	}

	@Test
	void testBeginAnd() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var bNames = ctx.getBindNames();
		assertEquals(2, bNames.size());
		assertEquals("bbb", bNames.get(0));
		assertEquals("ccc", bNames.get(1));
	}

	@Test
	void testIn() throws Exception {
		var sql = "SELECT * FROM emp WHERE deptno IN /*deptnoList*/(10, 20) ORDER BY ename";
		var sql2 = "SELECT * FROM emp WHERE deptno IN (?, ?)/*deptnoList*/ ORDER BY ename";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		List<Integer> deptnoList = new ArrayList<>();
		deptnoList.add(10);
		deptnoList.add(20);
		ctx.param("deptnoList", deptnoList);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(2, vars.length, "2");
		assertEquals(Integer.valueOf(10), vars[0], "3");
		assertEquals(Integer.valueOf(20), vars[1], "4");
	}

	@Test
	void testIn2() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(2, vars.length, "2");
		assertEquals(Integer.valueOf(10), vars[0], "3");
		assertEquals(Integer.valueOf(20), vars[1], "4");
	}

	@Test
	void testIn3() throws Exception {
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
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(4, vars.length, "2");
		assertEquals("SCOTT", vars[0], "3");
		assertEquals("MARY", vars[1], "4");
		assertEquals("ANALYST", vars[2], "5");
		assertEquals("FREE", vars[3], "6");
	}

	@Test
	void testParseBindVariable3() throws Exception {
		var sql = "BETWEEN sal ? AND ?";
		var sql2 = "BETWEEN sal ? AND ?"; // ? のパースは v1.0　で行わなくなった
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("$1", 0);
		ctx.param("$2", 1000);
		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql2, ctx.getExecutableSql(), "1");
	}

	@Test
	void testParseBindVariable4() throws Exception {
		var sql = "SELECT * FROM emp WHERE job = /*emp.job*/'CLERK' AND deptno = /*emp.deptno*/20";
		var sql2 = "SELECT * FROM emp WHERE job = ?/*emp.job*/ AND deptno = ?/*emp.deptno*/";
		var sql3 = "SELECT * FROM emp WHERE job = ";
		var sql4 = " AND deptno = ";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var emp = new Emp();
		emp.setJob("CLERK");
		emp.setDeptno(20);
		ctx.param("emp", emp);
		var transformer = parser.parse();
		transformer.transform(ctx);
		var root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");

		var vars = ctx.getBindVariables();
		assertEquals(2, vars.length, "2");
		assertEquals(emp.getJob(), vars[0], "3");
		assertEquals(emp.getDeptno(), vars[1], "4");
		assertEquals(4, root.getChildSize(), "5");
		var sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql(), "6");
		var varNode = (BindVariableNode) root.getChild(1);
		assertEquals("emp.job", varNode.getExpression(), "7");
		var sqlNode2 = (SqlNode) root.getChild(2);
		assertEquals(sql4, sqlNode2.getSql(), "8");
		var varNode2 = (BindVariableNode) root.getChild(3);
		assertEquals("emp.deptno", varNode2.getExpression(), "9");
	}

	@Test
	void testParseBindVariable5() throws Exception {
		var sql = "/*(count + 1)*/";
		var sql2 = "?/*(count + 1)*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("count", 3);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(1, vars.length, "2");
		assertEquals(4, vars[0], "3");
	}

	@Test
	void testParseBindVariable6() throws Exception {
		var sql = "SELECT * FROM EMP WHERE ENAME IN /*SF.split(enames, ',')*/()";
		var sql2 = "SELECT * FROM EMP WHERE ENAME IN (?, ?)/*SF.split(enames, ',')*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param(SqlFunction.SHORT_NAME, new SqlFunction(new DefaultDialect()));
		ctx.param("enames", "SCOTT,MARY");
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");
		var vars = ctx.getBindVariables();
		assertEquals(2, vars.length, "2");
		assertEquals("SCOTT", vars[0], "3");
		assertEquals("MARY", vars[1], "4");
	}

	@Test
	void testEndNotFound() throws Exception {
		var sql = "/*BEGIN*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		try {
			parser.parse();
			fail("1");
		} catch (EndCommentNotFoundRuntimeException ex) {
			System.out.println(ex);
		}
	}

	@Test
	void testEndParent() throws Exception {
		var sql = "INSERT INTO ITEM (ID, NUM) VALUES (/*id*/1, /*num*/20)";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("id", 0);
		ctx.param("num", 1);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(true, ctx.getExecutableSql().endsWith(")"), "1");
	}

	@Test
	void testEmbeddedValue() throws Exception {
		var sql = "xx /*#aaa*/ xx";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("aaa", 0);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals("xx '0'/*#aaa*/ xx", ctx.getExecutableSql(), "1");
	}

	@Test
	void testEmbeddedValue2() throws Exception {
		var sql = "/*$emp.deptno*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		var emp = new Emp();
		emp.setDeptno(0);
		ctx.param("emp", emp);
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals("0/*$emp.deptno*/", ctx.getExecutableSql(), "1");
	}

	/**
	 * 埋め込み変数のエスケープ処理テスト（SQLインジェクション対策）
	 *
	 * @throws Exception
	 */
	@Test
	void testEmbeddedValue3() throws Exception {
		var sql = "xx /*#aaa*/ xx";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("aaa", "bb'bb");
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals("xx 'bb''bb'/*#aaa*/ xx", ctx.getExecutableSql(), "1");
	}

	@Test
	void testStringFunction1() throws Exception {
		var sql = "/*IF SF.isEmpty(val1)*/1=1 /*ELIF SF.containsAny(val2, val3)*/2=2 --ELSE 3=3/*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("val1", null);
		ctx.param("val2", "aaabbbccc");
		ctx.param("val3", "ab");

		var sql2 = "1=1 ";
		ctx.param(SqlFunction.SHORT_NAME, new SqlFunction(new DefaultDialect()));
		var transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql(), "1");
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
	void testParamMissMatch1() throws Exception {
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
			fail("テスト失敗");
		} catch (ParameterNotFoundRuntimeException ex) {
			var msg = ex.getMessage();
			assertEquals("Parameter [val2] is not found.", msg, "1");
		} catch (Exception ex) {
			fail("期待しない例外. ex=" + ex.getMessage());
		}
	}

	@Test
	void testParamMissMatch2() throws Exception {
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
			fail("テスト失敗");
		} catch (ParameterNotFoundRuntimeException ex) {
			var msg = ex.getMessage();
			assertEquals("Parameter [val2, val3] is not found.", msg, "1");
		} catch (Exception ex) {
			fail("期待しない例外. ex=" + ex.getMessage());
		}
	}

	@Test
	void testParamMissMatch3() throws Exception {
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
			fail("期待しない例外. ex=" + ex.getMessage());
		}
	}

	@Test
	void testELSECOMMENT1() throws Exception {
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
		assertEquals(sql2, sql3);
	}

	@Test
	void testELSECOMMENT2() throws Exception {
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
		assertEquals(sql2, sql3);
	}

	@Test
	void testParseNodeDecrare() throws Exception {
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
	void testParseNode() throws Exception {
		var sql = "aaa /*IF purCd != null*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa  bbb", ctx.getExecutableSql());
	}

	// IF ENUM EQ
	@Test
	void testParseEnumEq1() throws Exception {
		var sql = "aaa /*IF CLS_TEST_ENUM1_A eq bindEnum*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("bindEnum", TestEnum1.A);

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa 1=1 bbb", ctx.getExecutableSql());
	}

	// IF ENUM ==
	@Test
	void testParseEnumEq2() throws Exception {
		var sql = "aaa /*IF CLS_TEST_ENUM1_A == bindEnum*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("bindEnum", TestEnum1.A);

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa 1=1 bbb", ctx.getExecutableSql());
	}

	// Delete AND / OR immediately after WHERE clause
	@Test
	void testDeleteImmediatelyAfterWhereClause() throws Exception {
		// OK Case
		var sql = "aaa Where /*IF true*/And col1 = 1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa Where col1 = 1 bbb", ctx.getExecutableSql());

		// OK Case in \r\n
		sql = "aaa Where\r\n/*IF true*/or\r\ncol1 = 1/*END*/ bbb";

		parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(), sqlConfig.getDialect().isRemoveTerminator(),
				true);
		ctx = sqlConfig.context();

		transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa Where\r\ncol1 = 1 bbb", ctx.getExecutableSql());

		// NG Case
		sql = "aaa WHERE /*IF true*/ORDER = 1/*END*/ bbb";

		parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(), sqlConfig.getDialect().isRemoveTerminator(),
				true);
		ctx = sqlConfig.context();

		transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa WHERE ORDER = 1 bbb", ctx.getExecutableSql());
	}

	@Test
	void testParseDontRemoveComma1() throws Exception {
		var sql = "SELECT /* _SQL_ID_ */ setval(/*sequenceName*/'', /*initialValue*/1, false)";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("sequenceName", "test_seq");
		ctx.param("initialValue", "1");

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("SELECT /* _SQL_ID_ */ setval(?/*sequenceName*/, ?/*initialValue*/, false)",
				ctx.getExecutableSql());
	}

	@Test
	void testParseDontRemoveComma2() throws Exception {
		var sql = "SELECT /* _SQL_ID_ */ TO_CHAR(FNC_ADD_MONTHS(TO_DATE(/*BIND_0*/'M', 'YYYYMMDD'), - 3), 'YYYYMMDD') AS ymd FROM test";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		var ctx = sqlConfig.context();
		ctx.param("BIND_0", "M");

		var transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(
				"SELECT /* _SQL_ID_ */ TO_CHAR(FNC_ADD_MONTHS(TO_DATE(?/*BIND_0*/, 'YYYYMMDD'), - 3), 'YYYYMMDD') AS ymd FROM test",
				ctx.getExecutableSql());
	}

}