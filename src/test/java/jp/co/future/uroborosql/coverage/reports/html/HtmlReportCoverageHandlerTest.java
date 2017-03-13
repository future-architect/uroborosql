package jp.co.future.uroborosql.coverage.reports.html;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.AbstractAgent;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;

public class HtmlReportCoverageHandlerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig("jdbc:h2:mem:coberturatestdb;DB_CLOSE_DELAY=-1;", "test", "test");

		try (SqlAgent agent = config.createAgent()) {

			SqlContext ctx = agent.contextWith("create table if not exists test ( \n id VARCHAR, name  VARCHAR \n )");
			agent.update(ctx);
		}
	}

	@After
	public void tearDown() {
	}

	@Test
	public void testReport() throws Exception {
		Path path = Paths.get("target", "coverage", "HtmlReportCoverageHandlerTest", "testReport");

		Files.deleteIfExists(path.resolve("example/select_test.html"));
		Files.deleteIfExists(path.resolve("covertest/test01.html"));
		Files.deleteIfExists(path.resolve("covertest/test02.html"));
		Files.deleteIfExists(path.resolve("covertest/test03.html"));

		//カバレッジ用インスタンスをクリア
		Field field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		AtomicReference<CoverageHandler> ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		CoverageHandler before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (SqlAgent agent = config.createAgent()) {
			agent.query("example/select_test").param("id", "A001").collect();

			agent.query("covertest/test01").param("id", 1).collect();
			agent.query("covertest/test01").collect();
			agent.query("covertest/test02").collect();
			agent.query("covertest/test03").collect();
		}
		WrapContextSqlFilter filter = new WrapContextSqlFilter("/* PREFIX */", "/* SUFFIX */",
				".*(FOR\\sUPDATE|\\.NEXTVAL).*");
		filter.initialize();
		config.getSqlFilterManager().addSqlFilter(filter);
		try (SqlAgent agent = config.createAgent()) {
			agent.query("covertest/test01").param("id", 1).collect();

		}

		assertThat(Files.exists(path.resolve("example/select_test.html")), is(true));

		assertThat(String.join("\n", Files.readAllLines(path.resolve("covertest/test01.html"))), is(
				"<!doctype html><html lang=\"en\"><head>    <title>Code coverage report for covertest/test01</title>    <meta charset=\"utf-8\" />    <style type=\"text/css\">table.coverage {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;}span.cline {width: 100%;display: inline-block;}span.cline.cline-no {background: #F6C6CE;}span.cline.cline-yes {background: rgb(230,245,208);}span.cline.no-target {background: #eaeaea;}.not-covered {background: #F6C6CE;}.not-covered-branch {background: yellow;}.source {vertical-align: top;}    </style></head><body><div class=\"header\">    <h1>covertest/test01</h1>    <div class=\"summary\">      <strong>85% </strong>      <span>Lines</span>      <span class=\"fraction\">12/14</span>    </div>    <div class=\"summary\">      <strong>66% </strong>      <span>Branches</span>      <span class=\"fraction\">4/6</span>    </div></div>    <pre>        <table class=\"coverage\">            <tr><td class=\"line-no\">1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18</td><td class=\"line-coverage\"><span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">2×</span>\n<span class=\"cline cline-yes\">2×</span></td><td class=\"source\">SELECT\n    *\nFROM\n    TEST    T\nWHERE\n    1       =   1\n/*IF SF.isNotEmpty(id) */\n    <span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF id &lt; 100 */</span>\nAND T.ID    =   /*id*/0\n    /*ELSE*/<span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.ID    =   100</span>\n    /*END*/\n/*END*/\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF SF.isNotEmpty(name) */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.NAME  =   /*name*/''</span>\n/*END*/\nORDER BY\n    T.ID\n</td>            </tr>        </table>    </pre></body>"));
		assertThat(String.join("\n", Files.readAllLines(path.resolve("covertest/test02.html"))), is(
				"<!doctype html><html lang=\"en\"><head>    <title>Code coverage report for covertest/test02</title>    <meta charset=\"utf-8\" />    <style type=\"text/css\">table.coverage {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;}span.cline {width: 100%;display: inline-block;}span.cline.cline-no {background: #F6C6CE;}span.cline.cline-yes {background: rgb(230,245,208);}span.cline.no-target {background: #eaeaea;}.not-covered {background: #F6C6CE;}.not-covered-branch {background: yellow;}.source {vertical-align: top;}    </style></head><body><div class=\"header\">    <h1>covertest/test02</h1>    <div class=\"summary\">      <strong>71% </strong>      <span>Lines</span>      <span class=\"fraction\">10/14</span>    </div>    <div class=\"summary\">      <strong>33% </strong>      <span>Branches</span>      <span class=\"fraction\">2/6</span>    </div></div>    <pre>        <table class=\"coverage\">            <tr><td class=\"line-no\">1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18</td><td class=\"line-coverage\"><span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span></td><td class=\"source\">SELECT\n    *\nFROM\n    TEST    T\nWHERE\n    1       =   1\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF SF.isNotEmpty(id) */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >    </span><span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF id &lt; 100 */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.ID    =   /*id*/0</span>\n    /*ELSE*/<span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.ID    =   100</span>\n    /*END*/\n/*END*/\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF SF.isNotEmpty(name) */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.NAME  =   /*name*/''</span>\n/*END*/\nORDER BY\n    T.ID\n</td>            </tr>        </table>    </pre></body>"));
		assertThat(String.join("\n", Files.readAllLines(path.resolve("covertest/test03.html"))), is(
				"<!doctype html><html lang=\"en\"><head>    <title>Code coverage report for covertest/test03</title>    <meta charset=\"utf-8\" />    <style type=\"text/css\">table.coverage {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;}span.cline {width: 100%;display: inline-block;}span.cline.cline-no {background: #F6C6CE;}span.cline.cline-yes {background: rgb(230,245,208);}span.cline.no-target {background: #eaeaea;}.not-covered {background: #F6C6CE;}.not-covered-branch {background: yellow;}.source {vertical-align: top;}    </style></head><body><div class=\"header\">    <h1>covertest/test03</h1>    <div class=\"summary\">      <strong>77% </strong>      <span>Lines</span>      <span class=\"fraction\">14/18</span>    </div>    <div class=\"summary\">      <strong>40% </strong>      <span>Branches</span>      <span class=\"fraction\">4/10</span>    </div></div>    <pre>        <table class=\"coverage\">            <tr><td class=\"line-no\">1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24</td><td class=\"line-coverage\"><span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline no-target\">&nbsp;</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span></td><td class=\"source\"><span class=\"not-covered-branch\" title=\"branch not covered\" >/*BEGIN*/</span>\n/* AAAAAAAAAAAA */\n/*END*/\nSELECT\n    *\nFROM\n    TEST    T\nWHERE\n    1       =   1\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF SF.isNotEmpty(id) */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >    </span><span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF id &lt; 100 */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.ID    =   /*id*/0</span>\n    /*ELSE*/<span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.ID    =   100</span>\n    /*END*/\n/*END*/\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF SF.isNotEmpty(name) */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.NAME  =   /*name*/''</span>\n/*END*/\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF true */</span>\n<span class=\"not-covered\" title=\"statement not covered\" >-- ELSEaaa</span>\n/*END*/\nORDER BY\n    T.ID\n</td>            </tr>        </table>    </pre></body>"));

		assertThat(String.join("\n", Files.readAllLines(path.resolve("index.html"))), is(
				"<!doctype html><html lang=\"en\"><head>    <title>uroboroSQL coverage report index</title>    <meta charset=\"utf-8\" />    <style type=\"text/css\">table.coverage-summary {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;width: 100%;}table.coverage-summary td {border: 1px solid #bbb;padding: 10px;}table.coverage-summary td.pic {min-width: 120px !important;}table.coverage-summary td.lines,table.coverage-summary td.lines-raw,table.coverage-summary td.branches,table.coverage-summary td.branches-raw {text-align: right;}table.coverage-summary td.pic>.chart {width: 100%;border: 1px solid #555;height: 1em;display: inline-block;}table.coverage-summary td.pic>.chart>div {display: inline-block;height: 100%;}table.coverage-summary td.pic>.chart>.cover-fill {background: rgb(77,146,33);}table.coverage-summary td.pic>.chart>.cover-empty {background: red;}    </style></head><body><div class=\"header\">    <h1>Summary</h1>    <div class=\"summary\">      <strong>81% </strong>      <span>Lines</span>      <span class=\"fraction\">57/70</span>    </div>    <div class=\"summary\">      <strong>46% </strong>      <span>Branches</span>      <span class=\"fraction\">15/32</span>    </div></div>    <pre>        <table class=\"coverage-summary\"><thead><tr>   <th class=\"file\" >File</th>   <th class=\"pic\" ></th>   <th class=\"lines\" >Lines</th>   <th class=\"lines-raw\"></th>   <th class=\"branches\" >Branches</th>   <th class=\"branches-raw\"></th></tr></thead><tbody><tr>   <td class=\"file\" ><a href=\"covertest/test01.html\" >covertest/test01</a></th>   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: 85%;\"></div><div class=\"cover-empty\" style=\"width:15%;\"></div></div></th>   <td class=\"lines\">85%</th>   <td class=\"lines-raw\">12/14</th>   <td class=\"branches\">66%</th>   <td class=\"branches-raw\">4/6</th></tr><tr>   <td class=\"file\" ><a href=\"covertest/test01_hash_1.html\" >covertest/test01_hash_1</a></th>   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: 86%;\"></div><div class=\"cover-empty\" style=\"width:14%;\"></div></div></th>   <td class=\"lines\">86%</th>   <td class=\"lines-raw\">13/15</th>   <td class=\"branches\">50%</th>   <td class=\"branches-raw\">3/6</th></tr><tr>   <td class=\"file\" ><a href=\"covertest/test02.html\" >covertest/test02</a></th>   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: 71%;\"></div><div class=\"cover-empty\" style=\"width:29%;\"></div></div></th>   <td class=\"lines\">71%</th>   <td class=\"lines-raw\">10/14</th>   <td class=\"branches\">33%</th>   <td class=\"branches-raw\">2/6</th></tr><tr>   <td class=\"file\" ><a href=\"covertest/test03.html\" >covertest/test03</a></th>   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: 77%;\"></div><div class=\"cover-empty\" style=\"width:23%;\"></div></div></th>   <td class=\"lines\">77%</th>   <td class=\"lines-raw\">14/18</th>   <td class=\"branches\">40%</th>   <td class=\"branches-raw\">4/10</th></tr><tr>   <td class=\"file\" ><a href=\"example/select_test.html\" >example/select_test</a></th>   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: 88%;\"></div><div class=\"cover-empty\" style=\"width:12%;\"></div></div></th>   <td class=\"lines\">88%</th>   <td class=\"lines-raw\">8/9</th>   <td class=\"branches\">50%</th>   <td class=\"branches-raw\">2/4</th></tr></tbody>        </table>    </pre></body>"));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}

	@Test
	public void testReportNoBranch() throws Exception {
		Path path = Paths.get("target", "coverage", "HtmlReportCoverageHandlerTest", "testReportNoBranch");

		Files.deleteIfExists(path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportNoBranch.html"));

		//カバレッジ用インスタンスをクリア
		Field field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		AtomicReference<CoverageHandler> ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		CoverageHandler before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (SqlAgent agent = config.createAgent()) {
			agent.query("covertest/HtmlReportCoverageHandlerTest/testReportNoBranch").collect();
		}

		assertThat(
				String.join("\n",
						Files.readAllLines(
								path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportNoBranch.html"))),
				is("<!doctype html><html lang=\"en\"><head>    <title>Code coverage report for covertest/HtmlReportCoverageHandlerTest/testReportNoBranch</title>    <meta charset=\"utf-8\" />    <style type=\"text/css\">table.coverage {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;}span.cline {width: 100%;display: inline-block;}span.cline.cline-no {background: #F6C6CE;}span.cline.cline-yes {background: rgb(230,245,208);}span.cline.no-target {background: #eaeaea;}.not-covered {background: #F6C6CE;}.not-covered-branch {background: yellow;}.source {vertical-align: top;}    </style></head><body><div class=\"header\">    <h1>covertest/HtmlReportCoverageHandlerTest/testReportNoBranch</h1>    <div class=\"summary\">      <strong>100% </strong>      <span>Lines</span>      <span class=\"fraction\">6/6</span>    </div>    <div class=\"summary\">      <strong>100% </strong>      <span>Branches</span>      <span class=\"fraction\">0/0</span>    </div></div>    <pre>        <table class=\"coverage\">            <tr><td class=\"line-no\">1\n2\n3\n4\n5\n6</td><td class=\"line-coverage\"><span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span></td><td class=\"source\">SELECT\n    *\nFROM\n    TEST    T\nORDER BY\n    T.ID\n</td>            </tr>        </table>    </pre></body>"));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}

	@Test
	public void testReportLastNoBranch() throws Exception {
		Path path = Paths.get("target", "coverage", "HtmlReportCoverageHandlerTest", "testReportLastNoBranch");

		Files.deleteIfExists(path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch.html"));

		//カバレッジ用インスタンスをクリア
		Field field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		AtomicReference<CoverageHandler> ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		CoverageHandler before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (SqlAgent agent = config.createAgent()) {
			agent.query("covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch").collect();
		}

		assertThat(
				String.join("\n",
						Files.readAllLines(
								path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch.html"))),
				is("<!doctype html><html lang=\"en\"><head>    <title>Code coverage report for covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch</title>    <meta charset=\"utf-8\" />    <style type=\"text/css\">table.coverage {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;}span.cline {width: 100%;display: inline-block;}span.cline.cline-no {background: #F6C6CE;}span.cline.cline-yes {background: rgb(230,245,208);}span.cline.no-target {background: #eaeaea;}.not-covered {background: #F6C6CE;}.not-covered-branch {background: yellow;}.source {vertical-align: top;}    </style></head><body><div class=\"header\">    <h1>covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch</h1>    <div class=\"summary\">      <strong>83% </strong>      <span>Lines</span>      <span class=\"fraction\">5/6</span>    </div>    <div class=\"summary\">      <strong>50% </strong>      <span>Branches</span>      <span class=\"fraction\">1/2</span>    </div></div>    <pre>        <table class=\"coverage\">            <tr><td class=\"line-no\">1\n2\n3\n4\n5\n6\n7</td><td class=\"line-coverage\"><span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-yes\">1×</span>\n<span class=\"cline cline-no\">&nbsp;</span>\n<span class=\"cline no-target\">&nbsp;</span></td><td class=\"source\">SELECT\n    *\nFROM\n    TEST    T\n<span class=\"not-covered-branch\" title=\"branch not covered\" >/*IF SF.isNotEmpty(name) */</span><span class=\"not-covered\" title=\"statement not covered\" ></span>\n<span class=\"not-covered\" title=\"statement not covered\" >AND T.NAME  =   /*name*/''</span>\n/*END*/\n</td>            </tr>        </table>    </pre></body>"));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}
}
