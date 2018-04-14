/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import jp.co.future.uroborosql.SqlAgent;

/**
 * Coberturaカバレッジレポート出力ハンドラ<br>
 * Jenkins Cobertura プラグイン で集計することができるレポートファイルを出力します。
 *
 * <pre>
 * デフォルトコンストラクタで生成される場合、レポートファイルの出力先は以下のように決定されます。
 *
 * sysytem property "uroborosql.sql.coverage.file" が指定された場合、指定されたPATHに xmlレポートを出力します。
 * 指定の無い場合、デフォルトで "./target/coverage/sql-cover.xml" に xmlレポートを出力します。
 * </pre>
 *
 * @author ota
 */
public class CoberturaCoverageHandler implements CoverageHandler {
	protected static final Logger LOG = LoggerFactory.getLogger(CoberturaCoverageHandler.class);

	/**
	 * カバレッジ数値 line branch セット
	 */
	private static class CoverageSummaryTotal {
		private final CoverageSummary line = new CoverageSummary();
		private final CoverageSummary branch = new CoverageSummary();

		private void add(final CoverageSummaryTotal total) {
			this.line.add(total.line);
			this.branch.add(total.branch);
		}
	}

	/**
	 * カバレッジ数値
	 */
	private static class CoverageSummary {
		private int valid;
		private int covered;

		private double getRate() {
			if (valid == 0) {
				return 0;
			}
			return (double) covered / (double) valid;
		}

		private void add(final CoverageSummary o) {
			this.valid += o.valid;
			this.covered += o.covered;
		}
	}

	/**
	 * ポイントブランチ情報
	 */
	private static class PointBranch {
		@SuppressWarnings("unused")
		private final Range range;
		private final Set<BranchCoverageState> status = EnumSet.noneOf(BranchCoverageState.class);

		private PointBranch(final Range range) {
			this.range = range;
		}

		private void add(final BranchCoverageState state) {
			status.add(state);
		}

		private int coveredSize() {
			return BranchCoverageState.getCoveredSize(status);
		}
	}

	/**
	 * 行ブランチ情報
	 */
	private static class LineBranch {
		@SuppressWarnings("unused")
		private final int rowIndxx;
		private final Map<Range, PointBranch> branches = new HashMap<>();

		private LineBranch(final int rowIndxx) {
			this.rowIndxx = rowIndxx;
		}

		private void add(final Range idx, final BranchCoverageState state) {
			PointBranch branch = branches.computeIfAbsent(idx, k -> new PointBranch(idx));

			branch.add(state);
		}

		private int branchSize() {
			return branches.size() * 2;
		}

		private int coveredSize() {
			return branches.values().stream().mapToInt(p -> p.coveredSize()).sum();
		}
	}

	/**
	 * SQL別カバレッジ元情報
	 */
	private static class SqlCoverage {
		private final String name;
		@SuppressWarnings("unused")
		private final String md5;
		/** 各行範囲 */
		private final List<LineRange> lineRanges;

		/** 各行ブランチカバレッジ情報 */
		private final Map<Integer, LineBranch> lineBranches = new HashMap<>();
		/** 各行通過回数 */
		private final int[] hitLines;

		private SqlCoverage(final String name, final String sql, final String md5, final Path sourcesDirPath,
				int hashIndex)
				throws IOException {
			this.name = hashIndex <= 0 ? name : name + "_hash_" + hashIndex;
			this.md5 = md5;
			this.lineRanges = CoverageHandler.parseLineRanges(sql);
			this.hitLines = new int[lineRanges.stream()
					.mapToInt(LineRange::getLineIndex)
					.max().orElse(0) + 1];
			writeSqlSource(sourcesDirPath, sql);
		}

		/**
		 * カバレッジ情報追加
		 *
		 * @param passRoute カバレッジ情報
		 */
		private void accept(final PassedRoute passRoute) {
			//各行の通過情報を集計
			for (LineRange range : lineRanges) {
				if (passRoute.isHit(range)) {
					hitLines[range.getLineIndex()]++;
				}
			}
			//各行のブランチ情報を集計
			passRoute.getRangeBranchStatus().forEach((range, state) -> {
				LineBranch lineBranch = lineBranches.computeIfAbsent(toRow(range), k -> new LineBranch(k));
				lineBranch.add(range, state);
			});
		}

		private int toRow(final Range target) {
			for (LineRange range : lineRanges) {
				if (range.hasIntersection(target)) {
					return range.getLineIndex();
				}
			}
			return -1;
		}

		/**
		 * SQL ファイル書き込み
		 *
		 * @param sourcesDirPath ディレクトリ
		 * @param sql SQL
		 * @throws IOException IOエラー
		 */
		private void writeSqlSource(final Path sourcesDirPath, final String sql) throws IOException {
			Path path = sourcesDirPath.resolve(name);
			Files.createDirectories(path.getParent());
			Files.write(path, sql.getBytes(StandardCharsets.UTF_8));
		}
	}

	/**
	 * パッケージで集約した情報
	 */
	private static class PackageSummary {
		private final String packagePath;
		private final List<SqlCoverage> coverageInfos = new ArrayList<>();

		private PackageSummary(final String packagePath) {
			this.packagePath = packagePath;
		}
	}

	private final Map<String, Map<String, SqlCoverage>> coverages = new ConcurrentHashMap<>();
	private final Path reportPath;
	private final Path sourcesDirPath;
	private boolean updated = true;

	/**
	 * コンストラクタ<br>
	 *
	 * <pre>
	 * sysytem property "uroborosql.sql.coverage.file" が指定された場合、指定されたPATHに xmlレポートを出力します。
	 * 指定の無い場合、デフォルトで "./target/coverage/sql-cover.xml" に xmlレポートを出力します。
	 * </pre>
	 */
	public CoberturaCoverageHandler() {
		String s = System.getProperty(SqlAgent.KEY_SQL_COVERAGE + ".file");
		if (StringUtils.isNotEmpty(s)) {
			this.reportPath = Paths.get(s);
		} else {
			this.reportPath = Paths.get("target", "coverage", "sql-cover.xml");
		}
		this.sourcesDirPath = this.reportPath.toAbsolutePath().getParent().resolve("sqls");
		init();
	}

	/**
	 * コンストラクタ
	 *
	 * @param reportPath レポートファイルPATH
	 */
	public CoberturaCoverageHandler(final Path reportPath) {
		this.reportPath = reportPath;
		this.sourcesDirPath = this.reportPath.toAbsolutePath().getParent().resolve("sqls");
		init();
	}

	@Override
	public synchronized void accept(final CoverageData coverageData) {
		if (StringUtils.isEmpty(coverageData.getSqlName())) {
			//SQL名の設定されていないSQLは集約しない
			return;
		}

		Map<String, SqlCoverage> map = coverages.computeIfAbsent(coverageData.getSqlName(),
				k -> new ConcurrentHashMap<>());
		SqlCoverage sqlCoverage = map.get(coverageData.getMd5());
		if (sqlCoverage == null) {
			try {
				sqlCoverage = new SqlCoverage(coverageData.getSqlName(), coverageData.getSql(), coverageData.getMd5(),
						sourcesDirPath, map.size());
			} catch (IOException e) {
				LOG.error(e.getMessage(), e);
				return;
			}
			map.put(coverageData.getMd5(), sqlCoverage);
		}

		sqlCoverage.accept(coverageData.getPassRoute());
		updated = true;
	}

	@Override
	public synchronized void onSqlAgentClose() {
		try {
			write();
		} catch (Exception e) {
			LOG.error(e.getMessage(), e);
		}
	}

	private void init() {
		//JVM終了時に書き込み
		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			try {
				write();
			} catch (Exception e) {
				LOG.error(e.getMessage(), e);
			}
		}));
	}

	synchronized void write() throws IOException, ParserConfigurationException, TransformerException {
		if (!updated) {//更新が無い場合は書き込みしない
			return;
		}

		List<PackageSummary> packageNodes = summaryPackages();

		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		Document document = documentBuilder.newDocument();

		Element coverage = document.createElement("coverage");

		coverage.setAttribute("timestamp", String.valueOf(System.currentTimeMillis()));
		coverage.setAttribute("complexity", "0");
		coverage.setAttribute("version", "0.1");

		document.appendChild(coverage);
		Element sources = document.createElement("sources");
		coverage.appendChild(sources);
		Element source = document.createElement("source");
		source.setTextContent(sourcesDirPath.toString());
		sources.appendChild(source);

		Element packages = document.createElement("packages");
		coverage.appendChild(packages);

		//packages内のrenderとカバレッジ集計
		CoverageSummaryTotal total = renderPackages(document, packages, packageNodes);

		CoverageSummary lines = total.line;
		coverage.setAttribute("lines-valid", String.valueOf(lines.valid));
		coverage.setAttribute("lines-covered", String.valueOf(lines.covered));
		coverage.setAttribute("lines-rate", String.valueOf(lines.getRate()));

		CoverageSummary branches = total.branch;
		coverage.setAttribute("branches-valid", String.valueOf(branches.valid));
		coverage.setAttribute("branches-covered", String.valueOf(branches.covered));
		coverage.setAttribute("branches-rate", String.valueOf(branches.getRate()));

		write(document);

		//書込が終了したので「更新なし」にする
		updated = false;
	}

	/**
	 * パッケージ単位にまとめる
	 *
	 * @return パッケージ単位情報
	 */
	private List<PackageSummary> summaryPackages() {
		Map<String, PackageSummary> summaries = new HashMap<>();

		coverages.forEach((name, c) -> {
			Path p = Paths.get(name).getParent();
			String pkg = p != null ? p.toString().replace(File.separatorChar, '.') : "_root_";
			PackageSummary summary = summaries.computeIfAbsent(pkg, k -> new PackageSummary(pkg));
			summary.coverageInfos.addAll(c.values());
		});
		return summaries.values().stream().sorted(Comparator.comparing(p -> p.packagePath))
				.collect(Collectors.toList());

	}

	private CoverageSummaryTotal renderPackages(final Document document, final Element packages,
			final List<PackageSummary> packageNodes) {
		CoverageSummaryTotal allTotal = new CoverageSummaryTotal();
		for (PackageSummary packageNode : packageNodes) {

			CoverageSummaryTotal total = new CoverageSummaryTotal();
			Element packageElm = document.createElement("package");
			packageElm.setAttribute("name", packageNode.packagePath);
			packages.appendChild(packageElm);

			Element classes = document.createElement("classes");
			packageElm.appendChild(classes);

			for (SqlCoverage coverageInfo : packageNode.coverageInfos) {
				//class内のrenderとカバレッジ集計
				total.add(renderClass(document, classes, coverageInfo));
			}
			packageElm.setAttribute("line-rate", String.valueOf(total.line.getRate()));
			packageElm.setAttribute("branch-rate", String.valueOf(total.branch.getRate()));

			allTotal.add(total);
		}

		return allTotal;
	}

	private CoverageSummaryTotal renderClass(final Document document, final Element classes,
			final SqlCoverage coverageInfo) {

		CoverageSummaryTotal total = new CoverageSummaryTotal();

		Element classElm = document.createElement("class");
		classElm.setAttribute("name", coverageInfo.name);
		classElm.setAttribute("filename", coverageInfo.name);
		classes.appendChild(classElm);

		Element methods = document.createElement("methods");
		classElm.appendChild(methods);

		Element lines = document.createElement("lines");
		classElm.appendChild(lines);

		total.line.valid = coverageInfo.lineRanges.size();
		for (LineRange range : coverageInfo.lineRanges) {
			int no = range.getLineIndex() + 1;
			int hit = coverageInfo.hitLines[range.getLineIndex()];
			if (hit > 0) {
				total.line.covered++;
			}

			Element line = document.createElement("line");
			lines.appendChild(line);
			line.setAttribute("number", String.valueOf(no));
			line.setAttribute("hits", String.valueOf(hit));
			LineBranch lineBranch = coverageInfo.lineBranches.get(range.getLineIndex());
			if (lineBranch != null) {
				int size = lineBranch.branchSize();
				int covered = lineBranch.coveredSize();
				line.setAttribute("branch", "true");
				line.setAttribute("condition-coverage",
						CoverageHandler.percentStr(covered, size) + "% (" + covered + "/" + size + ")");
				total.branch.valid += size;
				total.branch.covered += covered;

			} else {
				line.setAttribute("branch", "false");
			}
		}

		classElm.setAttribute("line-rate", String.valueOf(total.line.getRate()));
		classElm.setAttribute("branch-rate", String.valueOf(total.branch.getRate()));

		return total;
	}

	/**
	 * XML書き込み
	 *
	 * @param document xml document
	 * @throws IOException IOエラー
	 * @throws TransformerException XML書き込みエラー
	 */
	private void write(final Document document) throws IOException, TransformerException {
		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
		transformer
				.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, "http://cobertura.sourceforge.net/xml/coverage-04.dtd");
		try (BufferedWriter bufferedWriter = Files.newBufferedWriter(this.reportPath)) {
			transformer.transform(new DOMSource(document), new StreamResult(bufferedWriter));
		}
	}
}
