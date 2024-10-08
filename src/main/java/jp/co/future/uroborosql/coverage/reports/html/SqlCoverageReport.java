/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage.reports.html;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.coverage.LineRange;
import jp.co.future.uroborosql.coverage.PassedRoute;
import jp.co.future.uroborosql.coverage.Range;
import jp.co.future.uroborosql.coverage.Ranges;
import jp.co.future.uroborosql.log.support.CoverageLoggingSupport;
import jp.co.future.uroborosql.utils.ObjectUtils;

class SqlCoverageReport implements CoverageLoggingSupport {
	private final String name;
	private final String sql;
	private final Path path;
	private final Path reportDirPath;
	private final String md5;
	private boolean updated = true;
	/**
	 * 各行範囲
	 */
	private final List<LineRange> lineRanges;
	private final int sqlLineCount;

	/**
	 * ブランチカバレッジ情報
	 */
	private final Map<Range, RangeBranch> branches = new HashMap<>();

	/**
	 * 各行通過回数
	 */
	private final int[] hitLines;

	/**
	 * 各通過範囲
	 */
	private final Ranges passRanges = new Ranges();

	SqlCoverageReport(final String name, final String sql, final String md5, final Path reportDirPath,
			final int hashIndex) {
		this.name = hashIndex <= 0 ? name : name + "_hash_" + hashIndex;
		this.reportDirPath = reportDirPath;
		this.path = reportDirPath.resolve(this.name + ".html");
		this.sql = sql;
		this.md5 = md5;
		this.lineRanges = CoverageHandler.parseLineRanges(sql);
		this.sqlLineCount = CoverageHandler.getLineRanges(sql).size();
		this.hitLines = new int[this.sqlLineCount];
	}

	/**
	 * カバレッジ情報追加
	 *
	 * @param passRoute カバレッジ情報
	 */
	void accept(final PassedRoute passRoute) {
		//各行の通過情報を集計
		for (var range : lineRanges) {
			if (passRoute.isHit(range)) {
				hitLines[range.getLineIndex()]++;
			}
		}
		//各行のブランチ情報を集計
		passRoute.getRangeBranchStatus().forEach((range, state) -> {
			var branch = branches.computeIfAbsent(range, RangeBranch::new);
			branch.add(state);
		});

		//各通過情報を集計v
		passRanges.addAll(passRoute.getHitRanges());

		updated = true;

	}

	public String getMd5() {
		return md5;
	}

	public String getName() {
		return name;
	}

	void writeHtml() {
		if (!updated) {
			return;
		}
		try {
			Files.createDirectories(this.path.getParent());
			try (var writer = Files.newBufferedWriter(this.path, StandardCharsets.UTF_8)) {
				writePrefix(writer);

				writeHeaderSection(writer);

				writeTablePrefix(writer);
				writeLineNoSection(writer);
				writeHitsSection(writer);
				writeSourceSection(writer);
				writeTableSuffix(writer);

				writeSuffix(writer);
			}
		} catch (IOException ex) {
			errorWith(COVERAGE_LOG)
					.setMessage(ex.getMessage())
					.setCause(ex)
					.log();
		}
		updated = false;
	}

	private void writeLineNoSection(final BufferedWriter writer) throws IOException {
		writer.append("<td class=\"line-no\">");
		writer.newLine();
		writer.append("<pre>");
		writer.newLine();
		writer.append(IntStream.rangeClosed(1, this.sqlLineCount)
				.mapToObj(String::valueOf)
				.collect(Collectors.joining("\n")));
		writer.newLine();
		writer.append("</pre>");
		writer.newLine();
		writer.append("</td>");
		writer.newLine();
	}

	private void writeHitsSection(final BufferedWriter writer) throws IOException {
		writer.append("<td class=\"line-coverage\">");
		writer.append(IntStream.range(0, this.sqlLineCount)
				.mapToObj(i -> {
					if (!isTargetLine(i)) {
						return "<span class=\"cline no-target\">&nbsp;</span>";
					} else {
						var className = hitLines[i] > 0 ? "cline-yes" : "cline-no";
						var text = hitLines[i] > 0 ? hitLines[i] + "<em>×</em>" : "<em>!</em>";
						return "<span class=\"cline " + className + "\">" + text + "</span>";
					}
				})
				.collect(Collectors.joining("\n")));
		writer.append("</td>");
	}

	private boolean isTargetLine(final int index) {
		return lineRanges.stream()
				.mapToInt(LineRange::getLineIndex)
				.anyMatch(i -> i == index);
	}

	private void writeSourceSection(final BufferedWriter writer) throws IOException {
		writer.append("<td class=\"source\">");
		writer.newLine();
		writer.append("<pre>");
		writer.newLine();
		writer.append("<code class=\"sql\">");

		var inactives = new Ranges(0, this.sql.length() - 1);
		inactives.minus(lineRanges);
		var passes = this.passRanges.copy();
		passes.addAll(inactives);//カバレッジ対象でない行を通過したとみなす。
		passes.minus(branches.values().stream()
				.map(RangeBranch::getRange)
				.collect(Collectors.toList()));

		var start = 0;
		var pass = nextPassRange(passes, start);
		var branch = nextRangeBranch(start);

		while (pass != null && branch != null) {
			if (branch.getRange().getStart() <= pass.getStart()) {
				//branch
				start = appendBranch(writer, start, branch);
			} else {
				start = appendPassed(writer, start, pass);
			}
			pass = nextPassRange(passes, start);
			branch = nextRangeBranch(start);
		}
		while (pass != null) {
			start = appendPassed(writer, start, pass);
			pass = nextPassRange(passes, start);
		}
		while (branch != null) {
			start = appendBranch(writer, start, branch);
			branch = nextRangeBranch(start);
		}

		if (start < this.sql.length()) {
			appendNotCovered(writer, start, this.sql.length());
		}
		writer.append("</code>");
		writer.append("</pre>");
		writer.newLine();
		writer.append("</td>");
		writer.newLine();
	}

	private int appendBranch(final BufferedWriter writer, final int start, final RangeBranch branch)
			throws IOException {
		var range = branch.getRange();
		if (start < range.getStart()) {
			appendNotCovered(writer, start, range.getStart());
		}

		var size = branch.branchSize();
		var covered = branch.coveredSize();

		var html = size <= covered
				? buildLinesHtml(this.sql.substring(range.getStart(), range.getEnd() + 1), "", "")
				: buildLinesHtml(this.sql.substring(range.getStart(), range.getEnd() + 1),
						"<span class=\"not-covered-branch\" title=\"branch not covered\" >", "</span>");
		writer.append(html);
		return range.getEnd() + 1;
	}

	private int appendPassed(final BufferedWriter writer, final int start, final Range pass) throws IOException {

		if (start < pass.getStart()) {
			appendNotCovered(writer, start, pass.getStart());
		}
		var html = buildLinesHtml(this.sql.substring(Math.max(pass.getStart(), start), pass.getEnd() + 1),
				"", "");
		writer.append(html);
		return pass.getEnd() + 1;
	}

	private void appendNotCovered(final BufferedWriter writer, final int start, final int end) throws IOException {
		var html = buildLinesHtml(this.sql.substring(start, end),
				"<span class=\"not-covered\" title=\"statement not covered\" >", "</span>");
		writer.append(html);
	}

	private String buildLinesHtml(final String linesText, final String prefix, final String suffix) {
		return toLines(linesText).stream()
				.map(this::escapeHtml4)
				.map(s -> prefix + s + suffix)
				.collect(Collectors.joining("\n"));
	}

	private Range nextPassRange(final Ranges passes, final int start) {
		return passes.stream()
				.sorted()
				.filter(r -> start <= r.getEnd())
				.findFirst()
				.orElse(null);
	}

	private RangeBranch nextRangeBranch(final int start) {
		return branches.values().stream()
				.sorted(Comparator.comparing(RangeBranch::getRange))
				.filter(r -> start <= r.getRange().getEnd())
				.findFirst()
				.orElse(null);
	}

	private static List<String> toLines(final String text) {
		var ret = new ArrayList<String>();
		var s = text + "+";//最後の改行を検知するためダミー文字を付与
		try (var scanner = new Scanner(s)) {
			while (scanner.hasNextLine()) {
				var line = scanner.nextLine();
				ret.add(line);
			}
		}
		//ダミー文字除去
		var last = ret.get(ret.size() - 1);
		ret.set(ret.size() - 1, last.substring(0, last.length() - 1));

		return ret;
	}

	private void writePrefix(final BufferedWriter writer) throws IOException {
		writer.append("<!DOCTYPE html>");
		writer.newLine();
		writer.append("<html lang=\"en\">");
		writer.newLine();
		writer.append("<head>");
		writer.newLine();
		writer.append("    <meta charset=\"utf-8\" />");
		writer.newLine();
		writer.append("    <title>uroboroSQL code coverage report for ").append(this.name).append("</title>");
		writer.newLine();
		writer.append("    <link rel=\"stylesheet\" href=\"").append(getAssetPath()).append("/style.css\">");
		writer.newLine();
		writer.append("    <script src=\"").append(getAssetPath()).append("/jquery-3.2.0.min.js\"></script>");
		writer.newLine();
		writer.append("    <script src=\"").append(getAssetPath()).append("/highlight.pack.js\"></script>");
		writer.newLine();
		writer.append("    <script src=\"").append(getAssetPath()).append("/sqlcov.js\"></script>");
		writer.newLine();
		writer.append("</head>");
		writer.newLine();
		writer.append("<body>");
		writer.newLine();
	}

	private void writeHeaderSection(final BufferedWriter writer) throws IOException {
		var lineCount = getLineValidSize();
		var lineCovered = getLineCoveredSize();
		var branchesCount = getBranchValidSize();
		var branchesCovered = getBranchCoveredSize();

		writer.append("<div class=\"global-header\">");
		writer.newLine();
		writer.append("    <img class=\"icon\" src=\"").append(getAssetPath()).append("/icon.png\" />");
		writer.newLine();
		writer.append("    <span class=\"title\">uroboroSQL coverage</span>");
		writer.newLine();
		writer.append("</div>");
		writer.newLine();
		writer.append("<h1>").append(escapeHtml4(this.name)).append("</h1>");
		writer.newLine();
		writer.append("<div class=\"nav\">")
				.append("<a href=\"").append(getAssetPath()).append("/index.html\">All Files</a>")
				.append("</div>");
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

	private String getAssetPath() {
		return this.path.getParent().relativize(this.reportDirPath).toString();
	}

	private void writeTablePrefix(final BufferedWriter writer) throws IOException {
		writer.append("<table class=\"coverage\">");
		writer.newLine();
		writer.append("<tr>");
		writer.newLine();
	}

	private void writeTableSuffix(final BufferedWriter writer) throws IOException {
		writer.append("</tr>");
		writer.newLine();
		writer.append("</table>");
		writer.newLine();
	}

	private void writeSuffix(final BufferedWriter writer) throws IOException {
		writer.append("<div class=\"footer\">code coverage report generated by ")
				.append("<a href=\"https://github.com/future-architect/uroborosql\" target=\"_blank\">uroboroSQL</a> at ")
				.append(ZonedDateTime.now().format(DateTimeFormatter.ISO_ZONED_DATE_TIME))
				.append(".</div>");
		writer.newLine();
		writer.append("</body>");
		writer.newLine();
		writer.append("</html>");
		writer.newLine();
	}

	int getLineValidSize() {
		return lineRanges.size();
	}

	int getLineCoveredSize() {
		return (int) Arrays.stream(hitLines)
				.filter(i -> i > 0)
				.count();
	}

	int getBranchValidSize() {
		return branches.values().stream()
				.mapToInt(RangeBranch::branchSize)
				.sum();
	}

	int getBranchCoveredSize() {
		return branches.values().stream()
				.mapToInt(RangeBranch::coveredSize)
				.sum();
	}

	private String escapeHtml4(final String str) {
		if (ObjectUtils.isEmpty(str)) {
			return "";
		} else {
			return str.replace("\"", "&quot;")
					.replace("&", "&amp;")
					.replace("<", "&lt;")
					.replace(">", "&gt;");
		}
	}

}
