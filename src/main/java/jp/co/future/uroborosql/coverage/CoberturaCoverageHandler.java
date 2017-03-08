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
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Coberturaカバレッジレポート出力ハンドラ
 *
 * @author ota
 */
public class CoberturaCoverageHandler implements CoverageHandler {
	/**
	 * カバレッジ数値 line branch セット
	 */
	private static class CoverageTotalPack {
		private final CoverageTotal line = new CoverageTotal();
		private final CoverageTotal branch = new CoverageTotal();

		private void add(CoverageTotalPack pack) {
			this.line.add(pack.line);
			this.branch.add(pack.branch);
		}
	}

	/**
	 * カバレッジ数値
	 */
	private static class CoverageTotal {
		private int valid;
		private int covered;

		private double getRate() {
			if (valid == 0) {
				return 0;
			}
			return (double) covered / (double) valid;
		}

		private void add(CoverageTotal o) {
			this.valid += o.valid;
			this.covered += o.covered;
		}
	}

	/**
	 * SQL別カバレッジ元情報
	 */
	private static class CoverageInfo {
		private final String name;
		private final String sql;
		private final String md5;
		private final Map<Integer, Set<CoverageState>> rowStatus = new HashMap<>();
		private final List<LineRange> lineRanges;
		private final int[] hits;

		private CoverageInfo(String name, String sql, String md5, Path sourcesDirPath) throws IOException {
			this.name = name;
			this.sql = sql;
			this.md5 = md5;
			this.lineRanges = CoverageHandler.buildLineRanges(sql);
			this.hits = new int[this.lineRanges.size()];
			writeSqlSource(sourcesDirPath);
		}

		/**
		 * カバレッジ情報追加
		 *
		 * @param passRoute カバレッジ情報
		 */
		private void append(PassedRoute passRoute) {
			for (LineRange range : lineRanges) {
				if (passRoute.isHit(range)) {
					hits[range.getLineIndex()]++;
				}
			}
			passRoute.getPassed().forEach((idx, state) -> {
				rowStatus.computeIfAbsent(toRow(idx), k -> EnumSet.noneOf(CoverageState.class)).add(state);
			});
		}

		private int toRow(int idx) {
			for (int i = 0; i < lineRanges.size(); i++) {
				Range range = lineRanges.get(i);
				if (range.contains(idx)) {
					return i;
				}
			}
			return -1;
		}

		private void writeSqlSource(Path sourcesDirPath) throws IOException {
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
		private final List<CoverageInfo> coverageInfos = new ArrayList<>();

		public PackageSummary(String packagePath) {
			this.packagePath = packagePath;
		}

		public String getPackagePath() {
			return packagePath;
		}
	}

	private final Map<String, CoverageInfo> map = new ConcurrentHashMap<>();
	private final Path reportPath;
	private final Path sourcesDirPath;

	private long lastWriteTime;

	/**
	 * コンストラクタ
	 */
	public CoberturaCoverageHandler() {
		String s = System.getProperty("sql.coverage.file");
		if (StringUtils.isNotEmpty(s)) {
			this.reportPath = Paths.get(s);
		} else {
			this.reportPath = Paths.get("target", "coverage", "sql-clover.xml");
		}
		this.sourcesDirPath = this.reportPath.toAbsolutePath().getParent().resolve("sqls");
		init();
	}

	/**
	 * コンストラクタ
	 *
	 * @param reportPath レポートファイルPATH
	 */
	public CoberturaCoverageHandler(Path reportPath) {
		this.reportPath = reportPath;
		this.sourcesDirPath = this.reportPath.toAbsolutePath().getParent().resolve("sqls");
		init();
	}

	@Override
	public synchronized void accept(CoverageData coverageData) {
		if (StringUtils.isEmpty(coverageData.getSqlName())) {
			//SQL名の設定されていないSQLは集約しない
			return;
		}

		CoverageInfo info = map.get(coverageData.getSqlName());
		if (info == null || !info.md5.equals(coverageData.getMd5())) {
			try {
				info = new CoverageInfo(coverageData.getSqlName(), coverageData.getSql(), coverageData.getMd5(),
						sourcesDirPath);
			} catch (IOException e) {
				//ignore
				return;
			}
			map.put(coverageData.getSqlName(), info);
		}

		info.append(coverageData.getPassRoute());

		//リアルタイムな書き込みは10秒に1回とする
		if (lastWriteTime + 10000 < System.currentTimeMillis()) {
			try {
				write();
				lastWriteTime = System.currentTimeMillis();
			} catch (Exception e) {
				// ignore
			}
		}
	}

	private void init() {
		//JVM終了時に書き込み
		Runtime.getRuntime().addShutdownHook(new Thread(() -> {
			try {
				write();
			} catch (Exception e) {
				// ignore
			}
		}));
	}

	private void write() throws IOException, ParserConfigurationException, TransformerException {
		List<PackageSummary> packageNodes = summaryPackages();

		DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance()
				.newDocumentBuilder();
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
		CoverageTotalPack total = renderPackages(document, packages, packageNodes);

		CoverageTotal lines = total.line;
		coverage.setAttribute("lines-valid", String.valueOf(lines.valid));
		coverage.setAttribute("lines-covered", String.valueOf(lines.covered));
		coverage.setAttribute("lines-rate", String.valueOf(lines.getRate()));

		CoverageTotal branches = total.branch;
		coverage.setAttribute("branches-valid", String.valueOf(branches.valid));
		coverage.setAttribute("branches-covered", String.valueOf(branches.covered));
		coverage.setAttribute("branches-rate", String.valueOf(branches.getRate()));

		write(document);
	}

	/**
	 * パッケージ単位にまとめる
	 *
	 * @return パッケージ単位情報
	 */
	private List<PackageSummary> summaryPackages() {
		Map<String, PackageSummary> summaries = new HashMap<>();

		map.forEach((name, c) -> {
			Path p = Paths.get(name).getParent();
			String pkg = p != null ? p.toString().replace(File.separatorChar, '.') : "_root_";
			PackageSummary summary = summaries.computeIfAbsent(pkg, k -> new PackageSummary(pkg));
			summary.coverageInfos.add(c);
		});
		return summaries.values().stream()
				.sorted(Comparator.comparing(PackageSummary::getPackagePath))
				.collect(Collectors.toList());

	}

	private CoverageTotalPack renderPackages(Document document, Element packages, List<PackageSummary> packageNodes) {
		CoverageTotalPack allTotal = new CoverageTotalPack();
		for (PackageSummary packageNode : packageNodes) {

			CoverageTotalPack total = new CoverageTotalPack();
			Element packageElm = document.createElement("package");
			packageElm.setAttribute("name", packageNode.getPackagePath());
			packages.appendChild(packageElm);

			Element classes = document.createElement("classes");
			packageElm.appendChild(classes);

			for (CoverageInfo coverageInfo : packageNode.coverageInfos) {
				//class内のrenderとカバレッジ集計
				total.add(renderClass(document, classes, coverageInfo));
			}
			packageElm.setAttribute("line-rate", String.valueOf(total.line.getRate()));
			packageElm.setAttribute("branch-rate", String.valueOf(total.branch.getRate()));

			allTotal.add(total);
		}

		return allTotal;
	}

	private CoverageTotalPack renderClass(Document document, Element classes, CoverageInfo coverageInfo) {

		CoverageTotalPack total = new CoverageTotalPack();

		Element classElm = document.createElement("class");
		classElm.setAttribute("name", coverageInfo.name);
		classElm.setAttribute("filename", coverageInfo.name);
		classes.appendChild(classElm);

		Element methods = document.createElement("methods");
		classElm.appendChild(methods);

		Element lines = document.createElement("lines");
		classElm.appendChild(lines);

		for (int i = 0; i < coverageInfo.hits.length; i++) {
			int no = i + 1;
			int hit = coverageInfo.hits[i];
			total.line.valid++;
			if (hit > 0) {
				total.line.covered++;
			}

			Element line = document.createElement("line");
			lines.appendChild(line);
			line.setAttribute("number", String.valueOf(no));
			line.setAttribute("hits", String.valueOf(hit));
			Set<CoverageState> coverageStates = coverageInfo.rowStatus.get(i);
			if (coverageStates != null) {
				total.branch.valid++;
				line.setAttribute("branch", "true");
				line.setAttribute("condition-coverage",
						coverageStates.size() * 100 / 2 + "% (" + coverageStates.size() + "/2)");
				if (coverageStates.size() >= 2) {
					total.branch.covered++;
				}
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
	private void write(Document document) throws IOException, TransformerException {
		TransformerFactory transformerFactory = TransformerFactory
				.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
		transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM,
				"http://cobertura.sourceforge.net/xml/coverage-04.dtd");
		try (BufferedWriter bufferedWriter = Files.newBufferedWriter(this.reportPath)) {
			transformer.transform(new DOMSource(document), new StreamResult(bufferedWriter));
		}
	}
}
