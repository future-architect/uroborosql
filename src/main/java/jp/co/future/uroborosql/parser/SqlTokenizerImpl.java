/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parser;

import jp.co.future.uroborosql.exception.TokenNotClosedRuntimeException;

/**
 * SQL分割処理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlTokenizerImpl implements SqlTokenizer {
	/** SQL文 */
	private final String sql;

	/** 現在地 */
	private int position = 0;

	/** トークン */
	private String token;

	/** トークン種別 */
	private TokenType tokenType = TokenType.SQL;

	/** 次のトークン種別 */
	private TokenType nextTokenType = TokenType.SQL;

	/** バインド変数採番番号 */
	private int bindVariableNumber = 0;

	/**
	 * コンストラクタ
	 *
	 * @param sql 解析対象SQL
	 */
	public SqlTokenizerImpl(final String sql) {
		this.sql = sql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getPosition()
	 */
	@Override
	public int getPosition() {
		return position;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getToken()
	 */
	@Override
	public String getToken() {
		return token;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getBefore()
	 */
	@Override
	public String getBefore() {
		return sql.substring(0, position);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getAfter()
	 */
	@Override
	public String getAfter() {
		return sql.substring(position);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getTokenType()
	 */
	@Override
	public TokenType getTokenType() {
		return tokenType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getNextTokenType()
	 */
	@Override
	public TokenType getNextTokenType() {
		return nextTokenType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#next()
	 */
	@Override
	public TokenType next() {
		if (position >= sql.length()) {
			token = null;
			tokenType = TokenType.EOF;
			nextTokenType = TokenType.EOF;
			return tokenType;
		}
		switch (nextTokenType) {
		case SQL:
			parseSql();
			break;
		case COMMENT:
			parseComment();
			break;
		case ELSE:
			parseElse();
			break;
		case BIND_VARIABLE:
			parseBindVariable();
			break;
		default:
			parseEof();
			break;
		}
		return tokenType;
	}

	/**
	 * SQL文解析
	 */
	protected void parseSql() {
		var commentStartPos = sql.indexOf("/*", position);
		var lineCommentStartPos = sql.indexOf("--", position);
		var elseCommentStartPos = -1;
		if (lineCommentStartPos >= 0) {
			var skipPos = skipWhitespace(lineCommentStartPos + 2);
			if (skipPos + 4 < sql.length() && "ELSE".equals(sql.substring(skipPos, skipPos + 4))) {
				elseCommentStartPos = lineCommentStartPos;
			}
		}
		var nextStartPos = getNextStartPos(commentStartPos, elseCommentStartPos);
		if (nextStartPos < 0) {
			token = sql.substring(position);
			nextTokenType = TokenType.EOF;
			position = sql.length();
			tokenType = TokenType.SQL;
		} else {
			token = sql.substring(position, nextStartPos);
			tokenType = TokenType.SQL;
			var needNext = nextStartPos == position;
			if (nextStartPos == commentStartPos) {
				nextTokenType = TokenType.COMMENT;
				position = commentStartPos + 2;
			} else if (nextStartPos == elseCommentStartPos) {
				nextTokenType = TokenType.ELSE;
				position = elseCommentStartPos + 2;
			}
			if (needNext) {
				next();
			}
		}
	}

	/**
	 * 次の解析開始位置を取得
	 *
	 * @param commentStartPos コメント開始位置
	 * @param elseCommentStartPos ELSEコメント開始位置
	 * @return 次の解析位置
	 */
	protected int getNextStartPos(final int commentStartPos, final int elseCommentStartPos) {

		var nextStartPos = -1;
		if (commentStartPos >= 0) {
			nextStartPos = commentStartPos;
		}
		if (elseCommentStartPos >= 0 && (nextStartPos < 0 || elseCommentStartPos < nextStartPos)) {
			nextStartPos = elseCommentStartPos;
		}
		return nextStartPos;
	}

	/**
	 * 次のバインド変数名取得
	 *
	 * @return バインド変数名
	 */
	protected String nextBindVariableName() {
		return "$" + ++bindVariableNumber;
	}

	/**
	 * コメント部解析
	 */
	protected void parseComment() {
		var commentEndPos = sql.indexOf("*/", position);
		if (commentEndPos < 0) {
			throw new TokenNotClosedRuntimeException(sql.substring(position));
		}
		token = sql.substring(position, commentEndPos);
		nextTokenType = TokenType.SQL;
		position = commentEndPos + 2;
		tokenType = TokenType.COMMENT;
	}

	/**
	 * バインド変数解析
	 */
	protected void parseBindVariable() {
		token = nextBindVariableName();
		nextTokenType = TokenType.SQL;
		position += 1;
		tokenType = TokenType.BIND_VARIABLE;
	}

	/**
	 * ELSE文解析
	 */
	protected void parseElse() {
		var pos = sql.indexOf("ELSE", position) + 4;
		var elseToken = sql.substring(position, pos);
		token = elseToken;
		nextTokenType = TokenType.SQL;
		position = position + elseToken.length();
		tokenType = TokenType.ELSE;
	}

	/**
	 * EOF
	 */
	protected void parseEof() {
		token = null;
		tokenType = TokenType.EOF;
		nextTokenType = TokenType.EOF;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#skipToken()
	 */
	@Override
	public String skipToken() {
		var length = sql.length();
		var endIndex = length;
		var quote = position < length ? sql.charAt(position) : '\0';
		var quoting = quote == '\'' || quote == '(';
		if (quote == '(') {
			quote = ')';
		}
		for (var i = quoting ? position + 1 : position; i < length; ++i) {
			var c = sql.charAt(i);
			if ((Character.isWhitespace(c) || c == ',' || c == ')' || c == '(') && !quoting) {
				endIndex = i;
				break;
			} else if (c == '/' && i + 1 < length && sql.charAt(i + 1) == '*') {
				endIndex = i;
				break;
			} else if (c == '-' && i + 1 < length && sql.charAt(i + 1) == '-') {
				endIndex = i;
				break;
			} else if (quoting && quote == '\'' && c == '\'' && (i + 1 >= length || sql.charAt(i + 1) != '\'')) {
				endIndex = i + 1;
				break;
			} else if (quoting && c == quote) {
				endIndex = i + 1;
				break;
			}
		}
		token = sql.substring(position, endIndex);
		tokenType = TokenType.SQL;
		nextTokenType = TokenType.SQL;
		position = endIndex;
		return token;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#skipWhitespace()
	 */
	@Override
	public String skipWhitespace() {
		var index = skipWhitespace(position);
		token = sql.substring(position, index);
		position = index;
		return token;
	}

	/**
	 * ホワイトスペーススキップ
	 *
	 * @param position ポジション
	 * @return 空白をスキップした位置
	 */
	private int skipWhitespace(final int position) {
		var index = sql.length();
		for (var i = position; i < index; ++i) {
			var c = sql.charAt(i);
			if (!Character.isWhitespace(c)) {
				return i;
			}
		}
		return index;
	}

}
