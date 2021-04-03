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
		assertThat(transformed, is(expected));
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
		assertThat(ctx.getExecutableSql(), is(sql));
	}

	@Test
	public void testParseNormalComment() throws Exception {
		String sql = "SELECT /* empの全件検索 */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
	}

	@Test
	public void testParseHintComment() throws Exception {
		String sql = "SELECT /*+ FIRST_ROWS */ * FROM emp";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
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
		String sql = "SELECT * FROM emp";
		SqlParser parser = new SqlParserImpl(sql + endChar, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		assertThat(ctx.getExecutableSql(), is(sql));
	}

	@Test
	public void testCommentEndNotFound() throws Exception {
		String sql = "SELECT * FROM emp/*hoge";
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
		assertThat(ctx.getExecutableSql(), is(sql2));

		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(job));
		assertThat(vars[1], is(deptno));
		assertThat(root.getChildSize(), is(4));
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		BindVariableNode varNode = (BindVariableNode) root.getChild(1);
		assertThat(varNode.getExpression(), is("job"));
		SqlNode sqlNode2 = (SqlNode) root.getChild(2);
		assertThat(sqlNode2.getSql(), is(sql4));
		BindVariableNode varNode2 = (BindVariableNode) root.getChild(3);
		assertThat(varNode2.getExpression(), is("deptno"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		assertThat(root.getChildSize(), is(3));
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		SqlNode sqlNode2 = (SqlNode) root.getChild(1);
		assertThat(sqlNode2.getSql(), is(sql4));
		SqlNode sqlNode3 = (SqlNode) root.getChild(2);
		assertThat(sqlNode3.getSql(), is(sql5));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		SqlNode sqlNode = (SqlNode) root.getChild(2);
		assertThat(sqlNode.getSql(), is(sql3));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(job));
		assertThat(root.getChildSize(), is(2));
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		IfNode ifNode = (IfNode) root.getChild(1);
		assertThat(ifNode.getExpression(), is("job != null"));
		assertThat(ifNode.getChildSize(), is(2));
		SqlNode sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertThat(sqlNode2.getSql(), is(" WHERE job = "));
		BindVariableNode varNode = (BindVariableNode) ifNode.getChild(1);
		assertThat(varNode.getExpression(), is("job"));
		SqlContext ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertThat(ctx2.getExecutableSql(), is(sql3));
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
		SqlContext ctx2 = sqlConfig.context();
		ctx2.param("aaa", "hoge");
		ctx2.param("bbb", null);
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertThat(ctx2.getExecutableSql(), is("aaa"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(emp.getJob()));
		assertThat(root.getChildSize(), is(2));
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		IfNode ifNode = (IfNode) root.getChild(1);
		assertThat(ifNode.getExpression(), is("emp != null && emp.job != null"));
		assertThat(ifNode.getChildSize(), is(2));
		SqlNode sqlNode2 = (SqlNode) ifNode.getChild(0);
		assertThat(sqlNode2.getSql(), is(" WHERE job = "));
		BindVariableNode varNode = (BindVariableNode) ifNode.getChild(1);
		assertThat(varNode.getExpression(), is("emp.job"));
		SqlContext ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertThat(ctx2.getExecutableSql(), is(sql3));
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
		assertThat(ctx.getExecutableSql(), is(sql2));

		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(job));
		SqlContext ctx2 = sqlConfig.context();
		root.accept(ctx2);
		System.out.println("[" + ctx2.getExecutableSql() + "]");
		assertThat(ctx2.getExecutableSql(), is(sql3));
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
		assertThat(ctx.getExecutableSql(), is("bbb = ?/*bbb*/"));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(bbb));
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
		assertThat(ctx.getExecutableSql(), is("bbbddd"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		List<String> bNames = ctx.getBindNames();
		assertThat(bNames.size(), is(0));

		SqlContext ctx2 = sqlConfig.context();
		ctx2.param("job", "CLERK");
		ctx2.param("deptno", null);
		root.accept(ctx2);
		System.out.println(ctx2.getExecutableSql());
		assertThat(ctx2.getExecutableSql(), is(sql3));
		List<String> bNames2 = ctx2.getBindNames();
		assertThat(bNames2.size(), is(1));

		SqlContext ctx3 = sqlConfig.context();
		ctx3.param("job", "CLERK");
		ctx3.param("deptno", Integer.valueOf(20));
		root.accept(ctx3);
		System.out.println(ctx3.getExecutableSql());
		assertThat(ctx3.getExecutableSql(), is(sql4));
		List<String> bNames3 = ctx3.getBindNames();
		assertThat(bNames3.size(), is(2));

		SqlContext ctx4 = sqlConfig.context();
		ctx4.param("deptno", Integer.valueOf(20));
		ctx4.param("job", null);
		root.accept(ctx4);
		System.out.println(ctx4.getExecutableSql());
		assertThat(ctx4.getExecutableSql(), is(sql5));
		List<String> bNames4 = ctx4.getBindNames();
		assertThat(bNames4.size(), is(1));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		List<String> bNames = ctx.getBindNames();
		assertThat(bNames.size(), is(2));
		assertThat(bNames.get(0), is("bbb"));
		assertThat(bNames.get(1), is("ccc"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(Integer.valueOf(10)));
		assertThat(vars[1], is(Integer.valueOf(20)));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(Integer.valueOf(10)));
		assertThat(vars[1], is(Integer.valueOf(20)));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(4));
		assertThat(vars[0], is("SCOTT"));
		assertThat(vars[1], is("MARY"));
		assertThat(vars[2], is("ANALYST"));
		assertThat(vars[3], is("FREE"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(Integer.valueOf(0)));
		assertThat(vars[1], is(Integer.valueOf(1000)));
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
		assertThat(ctx.getExecutableSql(), is(sql2));

		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is(emp.getJob()));
		assertThat(vars[1], is(emp.getDeptno()));
		assertThat(root.getChildSize(), is(4));
		SqlNode sqlNode = (SqlNode) root.getChild(0);
		assertThat(sqlNode.getSql(), is(sql3));
		BindVariableNode varNode = (BindVariableNode) root.getChild(1);
		assertThat(varNode.getExpression(), is("emp.job"));
		SqlNode sqlNode2 = (SqlNode) root.getChild(2);
		assertThat(sqlNode2.getSql(), is(sql4));
		BindVariableNode varNode2 = (BindVariableNode) root.getChild(3);
		assertThat(varNode2.getExpression(), is("emp.deptno"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(1));
		assertThat(vars[0], is(4));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
		Object[] vars = ctx.getBindVariables();
		assertThat(vars.length, is(2));
		assertThat(vars[0], is("SCOTT"));
		assertThat(vars[1], is("MARY"));
	}

	@Test
	public void testEndNotFound() throws Exception {
		String sql = "/*BEGIN*/";
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
		String sql = "INSERT INTO ITEM (ID, NUM) VALUES (/*id*/1, /*num*/20)";
		SqlParser parser = new SqlParserImpl(sql, sqlConfig.getExpressionParser(),
				sqlConfig.getDialect().isRemoveTerminator(), true);
		SqlContext ctx = sqlConfig.context();
		ctx.param("id", Integer.valueOf(0));
		ctx.param("num", Integer.valueOf(1));
		ContextTransformer transformer = parser.parse();
		transformer.transform(ctx);
		System.out.println(ctx.getExecutableSql());
		assertThat(ctx.getExecutableSql().endsWith(")"), is(true));
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
		assertThat(ctx.getExecutableSql(), is("xx '0'/*#aaa*/ xx"));
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
		assertThat(ctx.getExecutableSql(), is("0/*$emp.deptno*/"));
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
		assertThat(ctx.getExecutableSql(), is("xx 'bb''bb'/*#aaa*/ xx"));
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
		assertThat(ctx.getExecutableSql(), is(sql2));
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
			assertThat("テスト失敗", false);
		} catch (ParameterNotFoundRuntimeException ex) {
			String msg = ex.getMessage();
			assertThat(msg, is("Parameter [val2] is not found."));
		} catch (Exception ex) {
			assertThat("期待しない例外. ex=" + ex.getMessage(), false);
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
			assertThat("テスト失敗", false);
		} catch (ParameterNotFoundRuntimeException ex) {
			String msg = ex.getMessage();
			assertThat(msg, is("Parameter [val2, val3] is not found."));
		} catch (Exception ex) {
			assertThat("期待しない例外. ex=" + ex.getMessage(), false);
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
			assertThat("期待しない例外. ex=" + ex.getMessage(), false);
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
		assertThat(sql3, is(sql2));
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
		assertThat(sql3, is(sql2));
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
		assertThat(ctx.getExecutableSql(), is("aaa  bbb"));
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
		assertThat(ctx.getExecutableSql(), is("aaa 1=1 bbb"));
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
		assertThat(ctx.getExecutableSql(), is("aaa 1=1 bbb"));
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