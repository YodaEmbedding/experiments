module.exports = {
  headers: {
    "X-Requested-With": "XMLHttpRequest", // so lila knows it's XHR
    "User-Agent": "CHANGE_THIS", // get this from your browser
    "Accept": '"*/*"',
    "Accept-Language": "en-US,en;q=0.5",
    "Accept-Encoding": "gzip, deflate, br",
    "Referer": "https://lichess.org/kZkqzsGN/black",  // random game link
    "X-Requested-With": "XMLHttpRequest",
    "Origin": "https://lichess.org",
    "DNT": "1",
    "Connection": "keep-alive",
    "Cookie": "CHANGE_THIS", // get this from your browser
    "Sec-Fetch-Dest": "empty",
    "Sec-Fetch-Mode": "cors",
    "Sec-Fetch-Site": "same-origin",
    "Content-Length": "0",
    "TE": "trailers",
  },
};
