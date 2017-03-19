package jp.co.future.uroborosql.coverage.reports.html;

import java.io.BufferedWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
		SqlCoverageReport sqlCoverage = map.computeIfAbsent(coverageData.getMd5(), k -> new SqlCoverageReport(coverageData.getSqlName(), coverageData.getSql(),
				coverageData.getMd5(), this.reportDirPath, map.size()));

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
			// copy resources
			String packagePath = this.getClass().getPackage().getName().replace(".", "/");
			Stream.of("style.css", "jquery-3.2.0.min.js", "stupidtable.min.js", "highlight.pack.js", "sqlcov.js", "icon.png").forEach(filename -> {
				try {
					Files.copy(Paths.get(this.getClass().getClassLoader().getResource(packagePath + "/" + filename).toURI()),
							Paths.get(this.reportDirPath + "/" + filename), StandardCopyOption.REPLACE_EXISTING);
				} catch (IOException | URISyntaxException e) {
					LOG.error(e.getMessage(), e);
				}
			});
			// write report
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
			writer.newLine();
			writer.append("   <td class=\"file\" ><a href=\"").append(linkName).append(".html\" >").append(htmlName)
					.append("</a></td>");
			writer.newLine();
			writer.append(
					"   <td class=\"pic\" ><div class=\"chart\"><div class=\"cover-fill\" style=\"width: ")
					.append(String.valueOf(lineCoveredPer))
					.append("%;\"></div><div class=\"cover-empty\" style=\"width:")
					.append(String.valueOf(100 - lineCoveredPer)).append("%;\"></div></div></td>");
			writer.newLine();
			writer.append("   <td class=\"lines\">").append(String.valueOf(lineCoveredPer))
					.append("%</td>");
			writer.newLine();
			writer.append("   <td class=\"lines-raw\">").append(String.valueOf(lineCovered)).append("/")
					.append(String.valueOf(lineCount)).append("</td>");
			writer.newLine();
			writer.append("   <td class=\"branches\">")
					.append(CoverageHandler.percentStr(branchesCovered, branchesCount))
					.append("%</td>");
			writer.newLine();
			writer.append("   <td class=\"branches-raw\">").append(String.valueOf(branchesCovered)).append("/")
					.append(String.valueOf(branchesCount)).append("</td>");
			writer.newLine();
			writer.append("</tr>");
			writer.newLine();
		}
	}

	private void writePrefix(BufferedWriter writer) throws IOException {
		writer.append("<!DOCTYPE html>");
		writer.newLine();
		writer.append("<html lang=\"en\">");
		writer.newLine();
		writer.append("<head>");
		writer.newLine();
		writer.append("    <meta charset=\"utf-8\" />");
		writer.newLine();
		writer.append("    <title>uroboroSQL code coverage report</title>");
		writer.newLine();
		writer.append("    <link rel=\"stylesheet\" href=\"style.css\">");
		writer.newLine();
		writer.append("    <script src=\"jquery-3.2.0.min.js\"></script>");
		writer.newLine();
		writer.append("    <script src=\"stupidtable.min.js\"></script>");
		writer.newLine();
		writer.append("    <script>$(function(){ $(\"table.coverage-summary\").stupidtable(); });</script>");
		writer.newLine();
		writer.append("</head>");
		writer.newLine();
		writer.append("<body>");
		writer.newLine();
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

		writer.append("<div class=\"global-header\">");
		writer.newLine();
		writer.append("    <img class=\"icon\" src=\"icon.png\" />");
		writer.newLine();
		writer.append("    <span class=\"title\">uroboroSQL coverage</span>");
		writer.newLine();
		writer.append("</div>");
		writer.newLine();
		writer.append("<h1>Code coverage report Summary</h1>");
		writer.newLine();
		writer.append("<div class=\"header\">");
		writer.newLine();
		writer.append("    <div class=\"summary\">");
		writer.newLine();
		writer.append("      <strong>").append(CoverageHandler.percentStr(lineCovered, lineCount))
				.append("% </strong>");
		writer.newLine();
		writer.append("      <span>Lines</span>");
		writer.newLine();
		writer.append("      <span class=\"fraction\">").append(String.valueOf(lineCovered)).append("/")
				.append(String.valueOf(lineCount)).append("</span>");
		writer.newLine();
		writer.append("    </div>");
		writer.newLine();
		writer.append("    <div class=\"summary\">");
		writer.newLine();
		writer.append("      <strong>").append(CoverageHandler.percentStr(branchesCovered, branchesCount))
				.append("% </strong>");
		writer.newLine();
		writer.append("      <span>Branches</span>");
		writer.newLine();
		writer.append("      <span class=\"fraction\">").append(String.valueOf(branchesCovered)).append("/")
				.append(String.valueOf(branchesCount)).append("</span>");
		writer.newLine();
		writer.append("    </div>");
		writer.newLine();
		writer.append("</div>");
		writer.newLine();
	}

	private void writeTablePrefix(BufferedWriter writer) throws IOException {
		writer.append("<div class=\"inner\">");
		writer.newLine();
		writer.append("<table class=\"coverage-summary\">");
		writer.newLine();
		writer.append("<thead>");
		writer.newLine();
		writer.append("<tr>");
		writer.newLine();
		writer.append("   <th data-sort=\"string\" class=\"file sorting-asc\" >File</th>");
		writer.newLine();
		writer.append("   <th class=\"pic\" ></th>");
		writer.newLine();
		writer.append("   <th data-sort=\"int\" class=\"lines\" >Lines</th>");
		writer.newLine();
		writer.append("   <th class=\"lines-raw\"></th>");
		writer.newLine();
		writer.append("   <th data-sort=\"int\" class=\"branches\" >Branches</th>");
		writer.newLine();
		writer.append("   <th class=\"branches-raw\"></th>");
		writer.newLine();
		writer.append("</tr>");
		writer.newLine();
		writer.append("</thead>");
		writer.newLine();
		writer.append("<tbody>");
		writer.newLine();

	}

	private void writeTableSuffix(BufferedWriter writer) throws IOException {
		writer.append("</tbody>");
		writer.newLine();
		writer.append("</table>");
		writer.newLine();
		writer.append("</div>");
		writer.newLine();
		writer.append("<div class=\"footer\">code coverage report generated by ")
				.append("<a href=\"https://github.com/future-architect/uroborosql\" target=\"_blank\">uroboroSQL</a> at ")
				.append(ZonedDateTime.now().format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
				.append(".</div>");
		writer.newLine();
	}

	private void writeSuffix(BufferedWriter writer) throws IOException {
		writer.append("</body>");
	}
}
