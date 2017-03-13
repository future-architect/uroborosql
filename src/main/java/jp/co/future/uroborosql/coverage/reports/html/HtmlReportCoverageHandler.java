package jp.co.future.uroborosql.coverage.reports.html;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.coverage.CoverageData;
import jp.co.future.uroborosql.coverage.CoverageHandler;

/**
 * カバレッジレポート出力ハンドラ<br>
 * htmlのカバレッジレポートを出力する
 *
 * <pre>
 * sysytem property "uroborosql.sql.coverage" に "jp.co.future.uroborosql.coverage.reports.html.HtmlReportCoverageHandler" を指定することで
 * 本機能を利用することができます。
 * </pre>
 *
 * @author ota
 */
public class HtmlReportCoverageHandler implements CoverageHandler {
	protected static final Logger LOG = LoggerFactory.getLogger(HtmlReportCoverageHandler.class);

	private final Map<String, Map<String, SqlCoverageReport>> coverages = new ConcurrentHashMap<>();
	private final Path reportDirPath;

	/**
	 * コンストラクタ<br>
	 *
	 * <pre>
	 * sysytem property "uroborosql.sql.coverage.dir" が指定された場合、指定されたPATHに レポートを出力します。
	 * 指定の無い場合、デフォルトで "./target/coverage/sql" に レポートを出力します。
	 * </pre>
	 */
	public HtmlReportCoverageHandler() {
		String s = System.getProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
		if (StringUtils.isNotEmpty(s)) {
			this.reportDirPath = Paths.get(s);
		} else {
			this.reportDirPath = Paths.get("target", "coverage", "sql");
		}
	}

	/**
	 * コンストラクタ
	 *
	 * @param reportDirPath レポートファイルPATH
	 */
	public HtmlReportCoverageHandler(Path reportDirPath) {
		this.reportDirPath = reportDirPath;
	}

	@Override
	public synchronized void accept(CoverageData coverageData) {
		if (StringUtils.isEmpty(coverageData.getSqlName())) {
			//SQL名の設定されていないSQLは集約しない
			return;
		}
		Map<String, SqlCoverageReport> map = coverages.computeIfAbsent(coverageData.getSqlName(),
				k -> new ConcurrentHashMap<>());
		SqlCoverageReport sqlCoverage = map.get(coverageData.getMd5());
		if (sqlCoverage == null) {
			sqlCoverage = new SqlCoverageReport(coverageData.getSqlName(), coverageData.getSql(),
					coverageData.getMd5(), this.reportDirPath, map.size());
			map.put(coverageData.getMd5(), sqlCoverage);
		}

		sqlCoverage.accept(coverageData.getPassRoute());
	}

	@Override
	public synchronized void onSqlAgentClose() {
		coverages.values().stream()
				.map(Map::values)
				.flatMap(Collection::stream)
				.forEach(SqlCoverageReport::writeHtml);
		writeHtml();
	}

	private void writeHtml() {
		try {
			Files.createDirectories(this.reportDirPath);
			try (BufferedWriter writer = Files.newBufferedWriter(this.reportDirPath.resolve("index.html"),
					StandardCharsets.UTF_8)) {
				writePrefix(writer);

				writeHeaderSection(writer);

				writeTablePrefix(writer);
				writeTable(writer);
				writeTableSuffix(writer);

				writeSuffix(writer);
			}
		} catch (IOException e) {
			LOG.error(e.getMessage(), e);
		}
	}

	private void writeTable(BufferedWriter writer) throws IOException {
		List<SqlCoverageReport> list = coverages.values().stream()
				.map(Map::values)
				.flatMap(Collection::stream)
				.sorted(Comparator.comparing(SqlCoverageReport::getName))
				.collect(Collectors.toList());
		for (SqlCoverageReport sqlCoverageReport : list) {

			String htmlName = StringEscapeUtils.escapeHtml4(sqlCoverageReport.getName());
			String linkName = sqlCoverageReport.getName();
			int lineCount = sqlCoverageReport.getLineValidSize();
			int lineCovered = sqlCoverageReport.getLineCoveredSize();

			int branchesCount = sqlCoverageReport.getBranchValidSize();
			int branchesCovered = sqlCoverageReport.getBranchCoveredSize();

			int lineCoveredPer = CoverageHandler.percent(lineCovered, lineCount);

			writer.append("<tr>");
			writer.append("   <td class=\"file\" ><a href=\"").append(linkName).append(".html\" >").append(htmlName)
					.append("</a></th>");
			writer.append(
					"   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: ")
					.append(String.valueOf(lineCoveredPer))
					.append("%;\"></div><div class=\"cover-empty\" style=\"width:")
					.append(String.valueOf(100 - lineCoveredPer)).append("%;\"></div></div></th>");
			writer.append("   <td class=\"lines\">").append(String.valueOf(lineCoveredPer))
					.append("%</th>");
			writer.append("   <td class=\"lines-raw\">").append(String.valueOf(lineCovered)).append("/")
					.append(String.valueOf(lineCount)).append("</th>");
			writer.append("   <td class=\"branches\">")
					.append(CoverageHandler.percentStr(branchesCovered, branchesCount))
					.append("%</th>");
			writer.append("   <td class=\"branches-raw\">").append(String.valueOf(branchesCovered)).append("/")
					.append(String.valueOf(branchesCount)).append("</th>");
			writer.append("</tr>");

		}
	}

	private void writePrefix(BufferedWriter writer) throws IOException {
		writer.append("<!doctype html>");
		writer.append("<html lang=\"en\">");
		writer.append("<head>");
		writer.append("    <title>uroboroSQL coverage report index</title>");
		writer.append("    <meta charset=\"utf-8\" />");
		writer.append("    <style type=\"text/css\">");

		writer.append("table.coverage-summary {border-collapse: collapse;margin: 10px 0 0 0;padding: 0;width: 100%;}");
		writer.append("table.coverage-summary td {border: 1px solid #bbb;padding: 10px;}");
		writer.append("table.coverage-summary td.pic {min-width: 120px !important;}");
		writer.append("table.coverage-summary td.lines,");
		writer.append("table.coverage-summary td.lines-raw,");
		writer.append("table.coverage-summary td.branches,");
		writer.append("table.coverage-summary td.branches-raw {text-align: right;}");

		writer.append(
				"table.coverage-summary td.pic>.chart {width: 100%;border: 1px solid #555;height: 1em;display: inline-block;}");
		writer.append("table.coverage-summary td.pic>.chart>div {display: inline-block;height: 100%;}");
		writer.append("table.coverage-summary td.pic>.chart>.cover-fill {background: rgb(77,146,33);}");
		writer.append("table.coverage-summary td.pic>.chart>.cover-empty {background: red;}");

		writer.append("    </style>");
		writer.append("</head>");
		writer.append("<body>");
	}

	private void writeHeaderSection(BufferedWriter writer) throws IOException {
		int lineCount = coverages.values().stream()
				.map(Map::values)
				.flatMap(Collection::stream)
				.mapToInt(SqlCoverageReport::getLineValidSize)
				.sum();
		int lineCovered = coverages.values().stream()
				.map(Map::values)
				.flatMap(Collection::stream)
				.mapToInt(SqlCoverageReport::getLineCoveredSize)
				.sum();

		int branchesCount = coverages.values().stream()
				.map(Map::values)
				.flatMap(Collection::stream)
				.mapToInt(SqlCoverageReport::getBranchValidSize)
				.sum();
		int branchesCovered = coverages.values().stream()
				.map(Map::values)
				.flatMap(Collection::stream)
				.mapToInt(SqlCoverageReport::getBranchCoveredSize)
				.sum();

		writer.append("<div class=\"header\">");
		writer.append("    <h1>Summary</h1>");
		writer.append("    <div class=\"summary\">");
		writer.append("      <strong>").append(CoverageHandler.percentStr(lineCovered, lineCount))
				.append("% </strong>");
		writer.append("      <span>Lines</span>");
		writer.append("      <span class=\"fraction\">").append(String.valueOf(lineCovered)).append("/")
				.append(String.valueOf(lineCount)).append("</span>");
		writer.append("    </div>");
		writer.append("    <div class=\"summary\">");
		writer.append("      <strong>").append(CoverageHandler.percentStr(branchesCovered, branchesCount))
				.append("% </strong>");
		writer.append("      <span>Branches</span>");
		writer.append("      <span class=\"fraction\">").append(String.valueOf(branchesCovered)).append("/")
				.append(String.valueOf(branchesCount)).append("</span>");
		writer.append("    </div>");
		writer.append("</div>");
	}

	private void writeTablePrefix(BufferedWriter writer) throws IOException {
		writer.append("    <pre>");
		writer.append("        <table class=\"coverage-summary\">");

		writer.append("<thead>");
		writer.append("<tr>");
		writer.append("   <th class=\"file\" >File</th>");
		writer.append("   <th class=\"pic\" ></th>");
		writer.append("   <th class=\"lines\" >Lines</th>");
		writer.append("   <th class=\"lines-raw\"></th>");
		writer.append("   <th class=\"branches\" >Branches</th>");
		writer.append("   <th class=\"branches-raw\"></th>");
		writer.append("</tr>");
		writer.append("</thead>");
		writer.append("<tbody>");

	}

	private void writeTableSuffix(BufferedWriter writer) throws IOException {
		writer.append("</tbody>");
		writer.append("        </table>");
		writer.append("    </pre>");

	}

	private void writeSuffix(BufferedWriter writer) throws IOException {
		writer.append("</body>");
	}
}
