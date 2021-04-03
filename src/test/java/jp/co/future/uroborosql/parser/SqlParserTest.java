package jp.co.future.uroborosql.parser;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.Emp;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.exception.EndCommentNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.ParameterNotFoundRuntimeException;
import jp.co.future.uroborosql.exception.TokenNotClosedRuntimeException;
import jp.co.future.uroborosql.node.BindVariableNode;
import jp.co.future.uroborosql.node.IfNode;
import jp.co.future.uroborosql.node.Node;
import jp.co.future.uroborosql.node.SqlNode;
import jp.co.future.uroborosql.utils.StringFunction;

public class SqlParserTest {
	private SqlConfig sqlConfig;

	@BeforeEach
	public void setUp() throws Exception {
		sqlConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setSqlContextFactory(new SqlContextFactoryImpl()
						.setEnumConstantPackageNames(Arrays.asList(TestEnum1.class.getPackage().getName())))
				.build();
	}

	private void sqlAssertion(final String original, final String expected) {
		SqlParser parser = new SqlParserImpl(original, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		String transformed = ctx.getExecutableSql().trim().replaceAll("\\s+", " ");
		assertEquals(expected, transformed);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElif() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		String sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF FALSE ELSE
	@Test
	public void testElif2() throws Exception {
		String sql = "/*IF true*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElif3() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4 --ELSE 3=3/*END*/";
		String sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELSE
	@Test
	public void testElif4() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3/*END*/";
		String sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	public void testElif4_1() throws Exception {
		String sql = "/*IF true*/1=1 /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE)
	@Test
	public void testElif5() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF true*/2=2/*END*/";
		String sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELSE (TRUE)
	@Test
	public void testElif6() throws Exception {
		String sql = "/*IF true*/1=1 /*ELIF true*/2=2/*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	@Test
	public void testElif7() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4/*END*/";
		String sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE
	@Test
	public void testElif8() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2/*END*/";
		String sql2 = "";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE)
	//  IF (TRUE)
	//  ELIF FALSE
	//  ELIF FALSE
	// ELSE
	@Test
	public void testElif_nest1() throws Exception {
		String sql = "/*IF true*/1=1 /*IF true*/and 11=11 /*ELIF false*/and 22=22/*END*/ /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		String sql2 = "1=1 and 11=11";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE)
	// IF FALSE
	// ELIF (TRUE)
	// ELIF FALSE
	// ELSE
	@Test
	public void testElif_nest2() throws Exception {
		String sql = "/*IF true*/1=1 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*/ /*ELIF true*/2=2 --ELSE 3=3/*END*/";
		String sql2 = "1=1 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF (TRUE)
	// IF (TRUE)
	// ELIF FALSE
	// ELSE
	@Test
	public void testElif_nest3() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*IF true*/and 11=11 /*ELIF false*/and 22=22/*END*/ --ELSE 3=3/*END*/";
		String sql2 = "2=2 and 11=11";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF (TRUE)
	// IF FALSE
	// ELIF (TRUE)
	// ELSE
	@Test
	public void testElif_nest4() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*/ --ELSE 3=3/*END*/";
		String sql2 = "2=2 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF FALSE
	// ELSE
	// IF (TRUE)
	// ELIF FALSE
	@Test
	public void testElif_nest5() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3 /*IF true*/and 11=11 /*ELIF false*/and 22=22/*END*//*END*/";
		String sql2 = "3=3 and 11=11";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE
	// ELIF FALSE
	// ELSE
	// IF FALSE
	// ELIF (TRUE)
	@Test
	public void testElif_nest6() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2  --ELSE 3=3 /*IF false*/and 11=11 /*ELIF true*/and 22=22/*END*//*END*/";
		String sql2 = "3=3 and 22=22";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE END
	@Test
	public void testBeginElif() throws Exception {
		String sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 /*END*//*END*/";
		String sql2 = "";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF (TRUE) ELIF FALSE END
	@Test
	public void testBeginElif2() throws Exception {
		String sql = "/*BEGIN*//*IF true*/1=1 /*ELIF false*/2=2 /*END*//*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF (TRUE) END
	@Test
	public void testBeginElif3() throws Exception {
		String sql = "/*BEGIN*//*IF false*/1=1 /*ELIF true*/2=2 /*END*//*END*/";
		String sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE ELSE END
	@Test
	public void testBeginElif4() throws Exception {
		String sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 --ELSE 3=3/*END*//*END*/";
		String sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// BEGIN IF FALSE ELIF FALSE ELSE END
	@Test
	public void testBeginElif5() throws Exception {
		String sql = "/*BEGIN*//*IF false*/1=1 /*ELIF false*/2=2 /*ELSE*/ 3=3/*END*//*END*/";
		String sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElifWithNewElse() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		String sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF FALSE ELSE
	@Test
	public void testElifWithNewElse2() throws Exception {
		String sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELIF (TRUE) ELSE
	@Test
	public void testElifWithNewElse3() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELIF true*/4=4 /*ELSE*/ 3=3/*END*/";
		String sql2 = "4=4";
		sqlAssertion(sql, sql2);
	}

	// IF FALSE ELIF FALSE ELSE
	@Test
	public void testElifWithNewElse4() throws Exception {
		String sql = "/*IF false*/1=1 /*ELIF false*/2=2 /*ELSE*/ 3=3/*END*/";
		String sql2 = "3=3";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	public void testElifWithNewElse4_1() throws Exception {
		String sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/ 3=3/*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF (TRUE) ELIF (TRUE) ELSE (ALL TRUE)
	@Test
	public void testElifWithNewElse4_2() throws Exception {
		String sql = "/*IF true*/1=1 /*ELIF true*/2=2 /*ELSE*/3=3/*END*/";
		String sql2 = "1=1";
		sqlAssertion(sql, sql2);
	}

	// IF -- COMMENT ELSE 2
	@Test
	public void testIFELSECOMMENT_2() throws Exception {
		String sql = "/*IF false*/1=1 -- comment /*ELIF true*/2=2 /*ELSE*/3=3/*END*/";
		String sql2 = "2=2";
		sqlAssertion(sql, sql2);
	}

	@Test
	public void testParse() throws Exception {
		String sql = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql());
	}

	@Test
	public void testParseNormalComment() throws Exception {
		String sql = "SELECT /* empの全件検索 */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql());
	}

	@Test
	public void testParseLineComment() throws Exception {
		String sql = "SELECT -- empの全件検索  * FROM emp WHERE job = /*job*/'CLERK'";
		String sql2 = "SELECT -- empの全件検索  * FROM emp WHERE job = ?/*job*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("job", "CLERK");
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql2, ctx.getExecutableSql());
	}

	@Test
	public void testParseHintComment() throws Exception {
		String sql = "SELECT /*+ FIRST_ROWS */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql());
	}

	@Test
	public void testParseEndSemicolon() throws Exception {
		testParseEndSemicolon(";");
		testParseEndSemicolon(";\t");
		testParseEndSemicolon("; ");
	}

	private void testParseEndSemicolon(final String endChar) {
		String sql = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql + endChar, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals(sql, ctx.getExecutableSql());
	}

	@Test
	public void testCommentEndNotFound() throws Exception {
		String sql = "SELECT * FROM emp/*hoge";
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
	public void testParseBindVariable() throws Exception {
		String sql = "SELECT * FROM emp WHERE job = /*job*/'CLERK' AND deptno = /*deptno*/20";
		String sql2 = "SELECT * FROM emp WHERE job = ?/*job*/ AND deptno = ?/*deptno*/";
		String sql3 = "SELECT * FROM emp WHERE job = ";
		String sql4 = " AND deptno = ";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		String job = "CLERK";
		Integer deptno = Integer.valueOf(20);
		ctx.param("job", job);
		ctx.param("deptno", deptno);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());

		Object[] vars = ctx.getBindVariables();
		assertEquals(2, vars.length);
		assertEquals(job, vars[0]);
		assertEquals(deptno, vars[1]);
		assertEquals(4, root.getChildSize());
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql());
		BindVariableNode varNode = (BindVariableNode) root.getChild(1);
		assertEquals("job", varNode.getExpression());
		SqlNode sqlNode2 = (SqlNode) root.getChild(2);
		assertEquals(sql4, sqlNode2.getSql());
		BindVariableNode varNode2 = (BindVariableNode) root.getChild(3);
		assertEquals("deptno", varNode2.getExpression());
	}

	@Test
	public void testParseBindVariable2() throws Exception {
		String sql = "SELECT * FROM emp WHERE job = /* job*/'CLERK'"; // コメントの先頭が空白ならコメントとして扱う
		String sql2 = "SELECT * FROM emp WHERE job = /* job*/'CLERK'";
		String sql3 = "SELECT * FROM emp WHERE job = ";
		String sql4 = "/* job*/";
		String sql5 = "'CLERK'";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		assertEquals(3, root.getChildSize());
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql());
		SqlNode sqlNode2 = (SqlNode) root.getChild(1);
		assertEquals(sql4, sqlNode2.getSql());
		SqlNode sqlNode3 = (SqlNode) root.getChild(2);
		assertEquals(sql5, sqlNode3.getSql());
	}

	@Test
	public void testParseWhiteSpace() throws Exception {
		String sql = "SELECT * FROM emp WHERE empno = /*empno*/1 AND 1 = 1";
		String sql2 = "SELECT * FROM emp WHERE empno = ?/*empno*/ AND 1 = 1";
		String sql3 = " AND 1 = 1";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		Integer empno = Integer.valueOf(7788);
		ctx.param("empno", empno);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		SqlNode sqlNode = (SqlNode) root.getChild(2);
		assertEquals(sql3, sqlNode.getSql());
	}

	@Test
	public void testParseIf() throws Exception {
		String sql = "SELECT * FROM emp/*IF job != null*/ WHERE job = /*job*/'CLERK'/*END*/";
		String sql2 = "SELECT * FROM emp WHERE job = ?/*job*/";
		String sql3 = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		String job = "CLERK";
		ctx.param("job", job);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(1, vars.length);
		assertEquals(job, vars[0]);
		assertEquals(2, root.getChildSize());
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql());
		IfNode ifNode = (IfNode) root.getChild(1);
		assertEquals("job != null", ifNode.getExpression());
		assertEquals(2, ifNode.getChildSize());
		SqlNode sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertEquals(" WHERE job = ", sqlNode2.getSql());
		BindVariableNode varNode = (BindVariableNode) ifNode.getChild(1);
		assertEquals("job", varNode.getExpression());
		SqlContext ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertEquals(sql3, ctx2.getExecutableSql());
	}

	@Test
	public void testParseIf2() throws Exception {
		String sql = "/*IF aaa != null*/aaa/*IF bbb != null*/bbb/*END*//*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("", ctx.getExecutableSql());
		ctx.param("aaa", null);
		ctx.param("bbb", "hoge");
		root.accept(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("", ctx.getExecutableSql());
		ctx.param("aaa", "hoge");
		root.accept(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("aaabbb", ctx.getExecutableSql());
		SqlContext ctx2 = sqlConfig.context();
		ctx2.param("aaa", "hoge");
		ctx2.param("bbb", null);
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertEquals("aaa", ctx2.getExecutableSql());
	}

	@Test
	public void testParseIf3() throws Exception {
		String sql = "SELECT * FROM emp/*IF emp != null && emp.job != null*/ WHERE job = /*emp.job*/'CLERK'/*END*/";
		String sql2 = "SELECT * FROM emp WHERE job = ?/*emp.job*/";
		String sql3 = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		Emp emp = new Emp();
		emp.setJob("CLERK");
		ctx.param("emp", emp);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(1, vars.length);
		assertEquals(emp.getJob(), vars[0]);
		assertEquals(2, root.getChildSize());
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql());
		IfNode ifNode = (IfNode) root.getChild(1);
		assertEquals("emp != null && emp.job != null", ifNode.getExpression());
		assertEquals(2, ifNode.getChildSize());
		SqlNode sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertEquals(" WHERE job = ", sqlNode2.getSql());
		BindVariableNode varNode = (BindVariableNode) ifNode.getChild(1);
		assertEquals("emp.job", varNode.getExpression());
		SqlContext ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertEquals(sql3, ctx2.getExecutableSql());
	}

	@Test
	public void testParseElse() throws Exception {
		String sql = "SELECT * FROM emp WHERE /*IF job != null*/job = /*job*/'CLERK'-- ELSE job is null/*END*/";
		String sql2 = "SELECT * FROM emp WHERE job = ?/*job*/";
		String sql3 = "SELECT * FROM emp WHERE job is null";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		String job = "CLERK";
		ctx.param("job", job);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals(sql2, ctx.getExecutableSql());

		Object[] vars = ctx.getBindVariables();
		assertEquals(1, vars.length);
		assertEquals(job, vars[0]);
		SqlContext ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertEquals(sql3, ctx2.getExecutableSql());
	}

	@Test
	public void testParseElse2() throws Exception {
		String sql = "/*IF false*/aaa--ELSE bbb = /*bbb*/123/*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		Integer bbb = Integer.valueOf(123);
		ctx.param("bbb", bbb);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("bbb = ?/*bbb*/", ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(1, vars.length);
		assertEquals(bbb, vars[0]);
	}

	@Test
	public void testParseElse3() throws Exception {
		String sql = "/*IF false*/aaa--ELSE bbb/*IF false*/ccc--ELSE ddd/*END*//*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals("bbbddd", ctx.getExecutableSql());
	}

	@Test
	public void testElse4() throws Exception {
		String sql = "SELECT * FROM emp/*BEGIN*/ WHERE /*IF false*/aaa-- ELSE AND deptno = 10/*END*//*END*/";
		String sql2 = "SELECT * FROM emp WHERE deptno = 10";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
	}

	@Test
	public void testBegin() throws Exception {
		String sql = "SELECT * FROM emp/*BEGIN*/ WHERE /*IF job != null*/job = /*job*/'CLERK'/*END*//*IF deptno != null*/ AND deptno = /*deptno*/20/*END*//*END*/";
		String sql2 = "SELECT * FROM emp";
		String sql3 = "SELECT * FROM emp WHERE job = ?/*job*/";
		String sql4 = "SELECT * FROM emp WHERE job = ?/*job*/ AND deptno = ?/*deptno*/";
		String sql5 = "SELECT * FROM emp WHERE  deptno = ?/*deptno*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);

		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		List<String> bNames = ctx.getBindNames();
		assertEquals(0, bNames.size());

		SqlContext ctx2 = sqlConfig.context();
		ctx2.param("job", "CLERK");
		ctx2.param("deptno", null);
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertEquals(sql3, ctx2.getExecutableSql());
		List<String> bNames2 = ctx2.getBindNames();
		assertEquals(1, bNames2.size());

		SqlContext ctx3 = sqlConfig.context();
		ctx3.param("job", "CLERK");
		ctx3.param("deptno", Integer.valueOf(20));
		root.accept(ctx3);
		System.out.println(ctx3.getExecutableSql());
		assertEquals(sql4, ctx3.getExecutableSql());
		List<String> bNames3 = ctx3.getBindNames();
		assertEquals(2, bNames3.size());

		SqlContext ctx4 = sqlConfig.context();
		ctx4.param("deptno", Integer.valueOf(20));
		ctx4.param("job", null);
		root.accept(ctx4);
		System.out.println(ctx4.getExecutableSql());
		assertEquals(sql5, ctx4.getExecutableSql());
		List<String> bNames4 = ctx4.getBindNames();
		assertEquals(1, bNames4.size());
	}

	@Test
	public void testBeginAnd() throws Exception {
		String sql = "/*BEGIN*/WHERE /*IF true*/aaa BETWEEN /*bbb*/111 AND /*ccc*/123/*END*//*END*/";
		String sql2 = "WHERE aaa BETWEEN ?/*bbb*/ AND ?/*ccc*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("bbb", "111");
		ctx.param("ccc", "222");
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println("[" + ctx.getExecutableSql() + "]");
		assertEquals(sql2, ctx.getExecutableSql());
		List<String> bNames = ctx.getBindNames();
		assertEquals(2, bNames.size());
		assertEquals("bbb", bNames.get(0));
		assertEquals("ccc", bNames.get(1));
	}

	@Test
	public void testIn() throws Exception {
		String sql = "SELECT * FROM emp WHERE deptno IN /*deptnoList*/(10, 20) ORDER BY ename";
		String sql2 = "SELECT * FROM emp WHERE deptno IN (?, ?)/*deptnoList*/ ORDER BY ename";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		List<Integer> deptnoList = new ArrayList<>();
		deptnoList.add(Integer.valueOf(10));
		deptnoList.add(Integer.valueOf(20));
		ctx.param("deptnoList", deptnoList);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(2, vars.length);
		assertEquals(Integer.valueOf(10), vars[0]);
		assertEquals(Integer.valueOf(20), vars[1]);
	}

	@Test
	public void testIn2() throws Exception {
		String sql = "SELECT * FROM emp WHERE deptno IN /*deptnoList*/(10, 20) ORDER BY ename";
		String sql2 = "SELECT * FROM emp WHERE deptno IN (?, ?)/*deptnoList*/ ORDER BY ename";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		int[] deptnoArray = { 10, 20 };
		ctx.param("deptnoList", deptnoArray);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(2, vars.length);
		assertEquals(Integer.valueOf(10), vars[0]);
		assertEquals(Integer.valueOf(20), vars[1]);
	}

	@Test
	public void testIn3() throws Exception {
		String sql = "SELECT * FROM emp WHERE ename IN /*enames*/('SCOTT','MARY') AND job IN /*jobs*/('ANALYST', 'FREE')";
		String sql2 = "SELECT * FROM emp WHERE ename IN (?, ?)/*enames*/ AND job IN (?, ?)/*jobs*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		String[] enames = { "SCOTT", "MARY" };
		String[] jobs = { "ANALYST", "FREE" };
		ctx.param("enames", enames);
		ctx.param("jobs", jobs);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(4, vars.length);
		assertEquals("SCOTT", vars[0]);
		assertEquals("MARY", vars[1]);
		assertEquals("ANALYST", vars[2]);
		assertEquals("FREE", vars[3]);
	}

	@Test
	public void testParseBindVariable3() throws Exception {
		String sql = "BETWEEN sal ? AND ?";
		String sql2 = "BETWEEN sal ?/*$1*/ AND ?/*$2*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("$1", Integer.valueOf(0));
		ctx.param("$2", Integer.valueOf(1000));
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(2, vars.length);
		assertEquals(Integer.valueOf(0), vars[0]);
		assertEquals(Integer.valueOf(1000), vars[1]);
	}

	@Test
	public void testParseBindVariable4() throws Exception {
		String sql = "SELECT * FROM emp WHERE job = /*emp.job*/'CLERK' AND deptno = /*emp.deptno*/20";
		String sql2 = "SELECT * FROM emp WHERE job = ?/*emp.job*/ AND deptno = ?/*emp.deptno*/";
		String sql3 = "SELECT * FROM emp WHERE job = ";
		String sql4 = " AND deptno = ";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		Emp emp = new Emp();
		emp.setJob("CLERK");
		emp.setDeptno(Integer.valueOf(20));
		ctx.param("emp", emp);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		Node root = transformer.getRoot();
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());

		Object[] vars = ctx.getBindVariables();
		assertEquals(2, vars.length);
		assertEquals(emp.getJob(), vars[0]);
		assertEquals(emp.getDeptno(), vars[1]);
		assertEquals(4, root.getChildSize());
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertEquals(sql3, sqlNode.getSql());
		BindVariableNode varNode = (BindVariableNode) root.getChild(1);
		assertEquals("emp.job", varNode.getExpression());
		SqlNode sqlNode2 = (SqlNode) root.getChild(2);
		assertEquals(sql4, sqlNode2.getSql());
		BindVariableNode varNode2 = (BindVariableNode) root.getChild(3);
		assertEquals("emp.deptno", varNode2.getExpression());
	}

	@Test
	public void testParseBindVariable5() throws Exception {
		String sql = "/*(count + 1)*/";
		String sql2 = "?/*(count + 1)*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("count", 3);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(1, vars.length);
		assertEquals(4, vars[0]);
	}

	@Test
	public void testParseBindVariable6() throws Exception {
		String sql = "SELECT * FROM EMP WHERE ENAME IN /*SF.split(enames, ',')*/()";
		String sql2 = "SELECT * FROM EMP WHERE ENAME IN (?, ?)/*SF.split(enames, ',')*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param(StringFunction.SHORT_NAME, new StringFunction(new DefaultDialect()));
		ctx.param("enames", "SCOTT,MARY");
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
		Object[] vars = ctx.getBindVariables();
		assertEquals(2, vars.length);
		assertEquals("SCOTT", vars[0]);
		assertEquals("MARY", vars[1]);
	}

	@Test
	public void testEndNotFound() throws Exception {
		String sql = "/*BEGIN*/";
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
	public void testEndParent() throws Exception {
		String sql = "INSERT INTO ITEM (ID, NUM) VALUES (/*id*/1, /*num*/20)";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("id", Integer.valueOf(0));
		ctx.param("num", Integer.valueOf(1));
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(true, ctx.getExecutableSql().endsWith(")"));
	}

	@Test
	public void testEmbeddedValue() throws Exception {
		String sql = "xx /*#aaa*/ xx";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("aaa", Integer.valueOf(0));
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals("xx '0'/*#aaa*/ xx", ctx.getExecutableSql());
	}

	@Test
	public void testEmbeddedValue2() throws Exception {
		String sql = "/*$emp.deptno*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		Emp emp = new Emp();
		emp.setDeptno(Integer.valueOf(0));
		ctx.param("emp", emp);
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals("0/*$emp.deptno*/", ctx.getExecutableSql());
	}

	/**
	 * 埋め込み変数のエスケープ処理テスト（SQLインジェクション対策）
	 *
	 * @throws Exception
	 */
	@Test
	public void testEmbeddedValue3() throws Exception {
		String sql = "xx /*#aaa*/ xx";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("aaa", "bb'bb");
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals("xx 'bb''bb'/*#aaa*/ xx", ctx.getExecutableSql());
	}

	@Test
	public void testStringFunction1() throws Exception {
		String sql = "/*IF SF.isEmpty(val1)*/1=1 /*ELIF SF.containsAny(val2, val3)*/2=2 --ELSE 3=3/*END*/";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("val1", null);
		ctx.param("val2", "aaabbbccc");
		ctx.param("val3", "ab");

		String sql2 = "1=1 ";
		ctx.param(StringFunction.SHORT_NAME, new StringFunction(new DefaultDialect()));
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertEquals(sql2, ctx.getExecutableSql());
	}

	@SuppressWarnings("deprecation")
	private void initData(final Connection conn) throws SQLException {
		// テーブル作成
		Statement stmt = conn.createStatement();
		stmt.execute("create table TEST( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE )");

		PreparedStatement pstmt = conn.prepareStatement("insert into test values (?, ?, ?, ?)");
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

		try (Connection conn = DriverManager.getConnection("jdbc:h2:mem:testParamMissMatch1")) {
			initData(conn);

			String sql = "select * from test where id = /*val1*/1 and name = /*val2*/'' and age = /*val3*/1";
			SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
					sqlConfig.getDialect().isRemoveTerminator(), true);
			SqlContext ctx = sqlConfig.context();
			ctx.param("val1", "1");
			ctx.param("val3", "20");

			ContextTransformer transformer = parser.parse();
			transformer.transform(ctx);

			st = conn.prepareStatement(ctx.getExecutableSql());
			ctx.bindParams(st);
			fail("テスト失敗");
		} catch (ParameterNotFoundRuntimeException ex) {
			String msg = ex.getMessage();
			assertEquals("Parameter [val2] is not found.", msg);
		} catch (Exception ex) {
			fail("期待しない例外. ex=" + ex.getMessage());
		}
	}

	@Test
	public void testParamMissMatch2() throws Exception {
		PreparedStatement st = null;

		try (Connection conn = DriverManager.getConnection("jdbc:h2:mem:testParamMissMatch2")) {
			initData(conn);

			String sql = "select * from test where id = /*val1*/1 and name = /*val2*/'' and age = /*val3*/1";
			SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
					sqlConfig.getDialect().isRemoveTerminator(), true);
			SqlContext ctx = sqlConfig.context();
			ctx.param("val1", "1");

			ContextTransformer transformer = parser.parse();
			transformer.transform(ctx);

			st = conn.prepareStatement(ctx.getExecutableSql());
			ctx.bindParams(st);
			fail("テスト失敗");
		} catch (ParameterNotFoundRuntimeException ex) {
			String msg = ex.getMessage();
			assertEquals("Parameter [val2, val3] is not found.", msg);
		} catch (Exception ex) {
			fail("期待しない例外. ex=" + ex.getMessage());
		}
	}

	@Test
	public void testParamMissMatch3() throws Exception {
		PreparedStatement st = null;

		try (Connection conn = DriverManager.getConnection("jdbc:h2:mem:testParamMissMatch3")) {
			initData(conn);

			String sql = "select * from test where id = /*val1*/1 and name = /*val2*/'' and age = /*val3*/1";
			SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
					sqlConfig.getDialect().isRemoveTerminator(), true);
			SqlContext ctx = sqlConfig.context();
			ctx.param("val1", "1");
			ctx.param("val2", "aa");
			ctx.param("val3", "20");

			ContextTransformer transformer = parser.parse();
			transformer.transform(ctx);

			st = conn.prepareStatement(ctx.getExecutableSql());
			ctx.bindParams(st);
		} catch (Exception ex) {
			fail("期待しない例外. ex=" + ex.getMessage());
		}
	}

	@Test
	public void testELSECOMMENT1() throws Exception {
		String sql = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT1.sql")),
				StandardCharsets.UTF_8);
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("param1", "1");

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		String sql2 = new String(
				Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT1.sql_expected")),
				StandardCharsets.UTF_8);
		String sql3 = ctx.getExecutableSql();
		System.out.println(sql3);
		assertEquals(sql2, sql3);
	}

	@Test
	public void testELSECOMMENT2() throws Exception {
		String sql = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT2.sql")),
				StandardCharsets.UTF_8);
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("param1", "2");

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		String sql2 = new String(
				Files.readAllBytes(Paths.get("src/test/resources/sql/test/ELSE_COMMENT2.sql_expected")),
				StandardCharsets.UTF_8);
		String sql3 = ctx.getExecutableSql();
		System.out.println(sql3);
		assertEquals(sql2, sql3);
	}

	@Test
	public void testParseNodeDecrare() throws Exception {
		String sql = "DECLARE /* _SQL_IDENTIFIER_ */	/*IF projectStage != \"dev\"*/	PRAGMA AUTONOMOUS_TRANSACTION;	/*END*/BEGIN	SELECT 'aaa' as AAA FROM DUAL;END;";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("projectStage", "ci");

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.format("%s\r\n", ctx.getExecutableSql());
	}

	// IF FALSE ELIF (TRUE) ELSE
	@Test
	public void testParseNode() throws Exception {
		String sql = "aaa /*IF purCd != null*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa  bbb", ctx.getExecutableSql());
	}

	// IF ENUM EQ
	@Test
	public void testParseEnumEq1() throws Exception {
		String sql = "aaa /*IF CLS_TEST_ENUM1_A eq bindEnum*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("bindEnum", TestEnum1.A);

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa 1=1 bbb", ctx.getExecutableSql());
	}

	// IF ENUM ==
	@Test
	public void testParseEnumEq2() throws Exception {
		String sql = "aaa /*IF CLS_TEST_ENUM1_A == bindEnum*/1=1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("bindEnum", TestEnum1.A);

		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertEquals("aaa 1=1 bbb", ctx.getExecutableSql());
	}

	// Delete AND / OR immediately after WHERE clause
	@Test
	public void testDeleteImmediatelyAfterWhereClause() throws Exception {
		// OK Case
		String sql = "aaa Where /*IF true*/And col1 = 1/*END*/ bbb";

		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();

		ContextTransformer transformer = parser.parse();
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

}