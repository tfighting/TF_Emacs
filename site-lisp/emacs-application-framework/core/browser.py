#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2018 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from PyQt5 import QtCore
from PyQt5.QtCore import QUrl, Qt, QEvent, QPointF, QEventLoop, QVariant, QTimer
from PyQt5.QtNetwork import QNetworkCookie
from PyQt5.QtWebEngineWidgets import QWebEngineView, QWebEnginePage, QWebEngineContextMenuData, QWebEngineProfile
from PyQt5.QtWidgets import QApplication, QWidget
from core.utils import touch, is_port_in_use
from core.buffer import Buffer
from urllib.parse import urlparse, parse_qs, urlunparse, urlencode
import os
import base64
import subprocess
import re

MOUSE_BACK_BUTTON = 8
MOUSE_FORWARD_BUTTON = 16

class BrowserView(QWebEngineView):

    open_url_in_new_tab = QtCore.pyqtSignal(str)
    open_url_in_background_tab = QtCore.pyqtSignal(str)
    translate_selected_text = QtCore.pyqtSignal(str)
    trigger_focus_event = QtCore.pyqtSignal(str)

    def __init__(self, config_dir):
        super(QWebEngineView, self).__init__()

        self.installEventFilter(self)
        self.config_dir = config_dir

        self.web_page = BrowserPage()
        self.setPage(self.web_page)

        self.cookie_store = self.page().profile().cookieStore()
        self.cookie_storage = BrowserCookieStorage(config_dir)
        self.cookie_store.cookieAdded.connect(self.cookie_storage.add_cookie)

        self.selectionChanged.connect(self.select_text_change)

        self.load_cookie()

        self.search_term = ""

        self.get_markers_raw = self.read_js_content("get_markers.js")
        self.goto_marker_raw = self.read_js_content("goto_marker.js")
        self.get_focus_text_js = self.read_js_content("get_focus_text.js")
        self.set_focus_text_raw = self.read_js_content("set_focus_text.js")
        self.clear_focus_js = self.read_js_content("clear_focus.js")
        self.select_input_text_js = self.read_js_content("select_input_text.js")
        self.dark_mode_js = self.read_js_content("dark_mode.js")

    def open_download_manage_page(self):
        self.open_url_new_buffer("file://" + (os.path.join(os.path.dirname(__file__), "aria2-webui", "index.html")))

    def read_js_content(self, js_file):
        return open(os.path.join(os.path.dirname(__file__), "js", js_file), "r").read()

    def filter_url(self, url):
        parsed = urlparse(url)
        qd = parse_qs(parsed.query, keep_blank_values=True)
        if parsed.netloc.startswith("www.google.com"):
            # Only save search parameters for Google, avoid duplicate url for same keyword.
            filtered = dict((k, v) for k, v in qd.items() if k.startswith("q"))
        else:
            filtered = dict((k, v) for k, v in qd.items())

        return urlunparse([
            parsed.scheme,
            parsed.netloc,
            parsed.path,
            parsed.params,
            urlencode(filtered, doseq=True), # query string
            parsed.fragment
        ])

    def _search_text(self, text, is_backward = False):
        if self.search_term != text:
            self.search_term = text
        if is_backward:
            self.web_page.findText(self.search_term, self.web_page.FindBackward)
        else:
            self.web_page.findText(self.search_term)

    def search_text_forward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Forward Search Text: ", "search_text_forward")
        else:
            self._search_text(self.search_term)

    def search_text_backward(self):
        if self.search_term == "":
            self.buffer.send_input_message("Backward Search Text: ", "search_text_backward")
        else:
            self._search_text(self.search_term, True)

    def search_quit(self):
        if self.search_term != "":
            self._search_text("")

    def select_text_change(self):
        modifiers = QApplication.keyboardModifiers()
        if modifiers == Qt.ControlModifier:
            self.translate_selected_text.emit(self.selectedText())

    def load_cookie(self):
        for cookie in self.cookie_storage.load_cookie():
            self.cookie_store.setCookie(cookie)

    def clear_cookies(self):
        self.cookie_storage.clear_cookies(self.cookie_store)

    def createWindow(self, window_type):
        return self.create_new_browser_window_callback()

    def event(self, event):
        if event.type() == QEvent.ChildAdded:
            obj = event.child()
            if isinstance(obj, QWidget):
                obj.installEventFilter(self)

        return QWebEngineView.event(self, event)

    def eventFilter(self, obj, event):
        # Focus emacs buffer when user click view.
        if event.type() in [QEvent.MouseButtonPress, QEvent.MouseButtonRelease,
                            QEvent.MouseMove, QEvent.MouseButtonDblClick, QEvent.Wheel]:
            # Send mouse event to applicatin view.
            self.trigger_focus_event.emit("{0},{1}".format(event.globalX(), event.globalY()))

        if event.type() == QEvent.MouseButtonRelease:
            hit = self.web_page.hitTestContent(event.pos())
            clicked_url = hit.linkUrl()
            base_url = hit.baseUrl()

            if clicked_url is not None and base_url is not None and clicked_url != base_url and clicked_url != '':
                result = ""

                if 'http://' in clicked_url or 'https://' in clicked_url:
                    result = clicked_url
                elif clicked_url == "#":
                    result = base_url + clicked_url
                else:
                    # Don't open url in EAF if clicked_url is not start with http/ftp or #
                    result = "http://" + base_url.split("/")[2] + clicked_url

                    event.accept()
                    return False

                modifiers = QApplication.keyboardModifiers()

                if modifiers == Qt.ControlModifier:
                    self.open_url_new_buffer(result)
                else:
                    self.open_url(result)

                return True

            event.accept()
            return False

        elif event.type() == QEvent.MouseButtonPress:
            if event.button() == MOUSE_FORWARD_BUTTON:
                self.forward()

                event.accept()
                return True
            elif event.button() == MOUSE_BACK_BUTTON:
                self.back()

                event.accept()
                return True

        return super(QWebEngineView, self).eventFilter(obj, event)

    def open_url(self, url):
        self.setUrl(QUrl(url))

    def open_url_new_buffer(self, url):
        self.open_url_in_new_tab.emit(url)

    def open_url_background_buffer(self, url):
        self.open_url_in_background_tab.emit(url)

    def zoom_in(self):
        self.setZoomFactor(min(5, self.zoomFactor() + 0.25))

    def zoom_out(self):
        self.setZoomFactor(max(0.25, self.zoomFactor() - 0.25))

    def zoom_reset(self):
        self.setZoomFactor(1)

    def eval_js(self, js):
        self.web_page.runJavaScript(js)

    def eval_js_file(self, js_file):
        self.eval_js(self.read_js_content(js_file))

    def execute_js(self, js):
        return self.web_page.executeJavaScript(js)

    def scroll_left(self):
        self.eval_js("document.scrollingElement.scrollBy(-35, 0)")

    def scroll_right(self):
        self.eval_js("document.scrollingElement.scrollBy(35, 0)")

    def scroll_up(self):
        self.eval_js("document.scrollingElement.scrollBy(0, 50)")

    def scroll_down(self):
        self.eval_js("document.scrollingElement.scrollBy(0, -50)")

    def scroll_up_page(self):
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def scroll_down_page(self):
        self.eval_js("document.scrollingElement.scrollBy({left: 0, top: -window.innerHeight/2, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def scroll_to_begin(self):
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: 0, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def scroll_to_bottom(self):
        self.eval_js("document.scrollingElement.scrollTo({left: 0, top: document.body.scrollHeight, behavior: '" + self.buffer.emacs_var_dict["eaf-browser-scroll-behavior"] + "'})")

    def refresh_page(self):
        self.reload()

    def copy_text(self):
        self.triggerPageAction(self.web_page.Copy)

    def yank_text(self):
        self.triggerPageAction(self.web_page.Paste)

    def kill_text(self):
        self.triggerPageAction(self.web_page.Cut)

    def undo_action(self):
        self.triggerPageAction(self.web_page.Undo)

    def redo_action(self):
        self.triggerPageAction(self.web_page.Redo)

    def select_all(self):
        # We need window focus before select all text.
        self.execute_js("window.focus()")
        self.triggerPageAction(self.web_page.SelectAll)

    def select_input_text(self):
        self.execute_js(self.select_input_text_js)

    def get_url(self):
        return self.execute_js("window.location.href;")

    def cleanup_links(self):
        self.eval_js("document.querySelector('.eaf-marker-container').remove();")
        self.eval_js("document.querySelector('.eaf-style').remove();")

    def get_link_markers(self):
        return self.execute_js(self.get_markers_raw.replace("%1", self.buffer.emacs_var_dict["eaf-marker-letters"]));

    def jump_to_link(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.execute_js(self.goto_marker_js)
        self.cleanup_links()
        if link != "":
            self.open_url(link)

    def jump_to_link_new_buffer(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.execute_js(self.goto_marker_js)
        self.cleanup_links()
        if link != "":
            self.open_url_new_buffer(link)

    def jump_to_link_background_buffer(self, marker):
        self.goto_marker_js = self.goto_marker_raw.replace("%1", str(marker));
        link = self.execute_js(self.goto_marker_js)
        self.cleanup_links()
        if link != "":
            self.open_url_background_buffer(link)

    def get_focus_text(self):
        return self.execute_js(self.get_focus_text_js)

    def set_focus_text(self, new_text):
        self.set_focus_text_js = self.set_focus_text_raw.replace("%1", str(base64.b64encode(new_text.encode("utf-8")), "utf-8"));
        self.execute_js(self.set_focus_text_js)

    def clear_focus(self):
        self.execute_js(self.clear_focus_js)

    def dark_mode(self):
        self.execute_js(self.dark_mode_js)

class BrowserPage(QWebEnginePage):
    def __init__(self):
        QWebEnginePage.__init__(self)

    def hitTestContent(self, pos):
        return WebHitTestResult(self, pos)

    def mapToViewport(self, pos):
        return QPointF(pos.x(), pos.y())

    def executeJavaScript(self, scriptSrc):
        self.loop = QEventLoop()
        self.result = QVariant()
        QTimer.singleShot(250, self.loop.quit)

        self.runJavaScript(scriptSrc, self.callbackJS)
        self.loop.exec_()
        self.loop = None
        return self.result

    def callbackJS(self, res):
        if self.loop is not None and self.loop.isRunning():
            self.result = res
            self.loop.quit()

class WebHitTestResult():
    def __init__(self, page, pos):
        self.page = page
        self.pos = pos
        self.m_linkUrl = self.page.url().toString()
        self.m_baseUrl = self.page.url().toString()
        self.viewportPos = self.page.mapToViewport(self.pos)
        with open(os.path.join(os.path.dirname(__file__), "js", "open_in_new_tab.js"), "r") as f:
            self.open_in_new_tab_raw = f.read()

        self.open_in_new_tab_js = self.open_in_new_tab_raw.replace("%1", str(self.viewportPos.x())).replace("%2", str(self.viewportPos.y()))
        self.dic = self.page.executeJavaScript(self.open_in_new_tab_js)
        if self.dic is None:
            return

        self.m_isNull = False
        self.m_baseUrl = self.dic["baseUrl"]
        self.m_alternateText = self.dic["alternateText"]
        self.m_imageUrl = self.dic["imageUrl"]
        self.m_isContentEditable = self.dic["contentEditable"]
        self.m_isContentSelected = self.dic["contentSelected"]
        self.m_linkTitle = self.dic["linkTitle"]
        self.m_linkUrl = self.dic["linkUrl"]
        self.m_mediaUrl = self.dic["mediaUrl"]
        try:
            self.m_mediaPaused = self.dic["mediaPaused"]
            self.m_mediaMuted = self.dic["mediaMuted"]
        except Exception:
            pass
        self.m_tagName = self.dic["tagName"]

    def linkUrl(self):
        return self.m_linkUrl

    def isContentEditable(self):
        return self.m_isContentEditable

    def isContentSelected(self):
        return self.m_isContentSelected

    def imageUrl(self):
        try:
            return self.m_imageUrl
        except Exception:
            return ""

    def mediaUrl(self):
        return self.m_mediaUrl

    def baseUrl(self):
        return self.m_baseUrl

    def updateWithContextMenuData(self, data):
        if data.isValid():
            pass
        else:
            return

        self.m_linkTitle = data.linkText()
        self.m_linkUrl = data.linkUrl().toString()
        self.m_isContentEditable = data.isContentEditable()
        if data.selectedText() == "":
            self.m_isContentSelected = False
        else:
            self.m_isContentSelected = True

        if data.mediaType() == QWebEngineContextMenuData.MediaTypeImage:
            self.m_imageUrl = data.mediaUrl().toString()
        elif data.mediaType() == QWebEngineContextMenuData.MediaTypeAudio or data.mediaType() == QWebEngineContextMenuData.MediaTypeVideo:
            self.m_mediaUrl = data.mediaUrl().toString()

class BrowserCookieStorage:
    def __init__(self, config_dir):
        self.cookie_file = os.path.join(config_dir, "browser", "cookie", "cookie")

        touch(self.cookie_file)

    def load_cookie(self):
        with open(self.cookie_file, 'rb+') as store:
            cookies = store.read()
            return QNetworkCookie.parseCookies(cookies)

    def save_cookie(self, cookie):
        with open(self.cookie_file, 'wb+') as store:
            store.write(cookie + b'\n' if cookie is not None else b'')

    def add_cookie(self, cookie):
        raw = cookie.toRawForm()
        self.save_cookie(raw)

    def clear_cookies(self, cookie_store):
        cookie_store.deleteAllCookies()

        open(self.cookie_file, 'w').close()

class BrowserBuffer(Buffer):

    close_page = QtCore.pyqtSignal(str)
    get_focus_text = QtCore.pyqtSignal(str, str)

    def __init__(self, buffer_id, url, config_dir, arguments, emacs_var_dict, fit_to_view, background_color):
        Buffer.__init__(self, buffer_id, url, arguments, emacs_var_dict, fit_to_view, background_color)

        self.add_widget(BrowserView(config_dir))

        self.config_dir = config_dir
        self.history_log_file_path = os.path.join(self.config_dir, "browser", "history", "log.txt")
        self.history_url_pattern = re.compile("(.*?)\s([^\s]+)$")
        self.history_close_file_path = os.path.join(self.config_dir, "browser", "history", "close.txt")

        # Set User Agent with Firefox's one to make EAF browser can login in Google account.
        self.profile = QWebEngineProfile(self.buffer_widget)
        self.profile.defaultProfile().setHttpUserAgent("Mozilla/5.0 (X11; Linux i586; rv:31.0) Gecko/20100101 Firefox/72.0")

        self.buffer_widget.loadStarted.connect(self.start_progress)
        self.buffer_widget.loadProgress.connect(self.update_progress)
        self.buffer_widget.loadFinished.connect(self.stop_progress)

        self.buffer_widget.web_page.windowCloseRequested.connect(self.request_close_buffer)

        self.profile.defaultProfile().downloadRequested.connect(self.handle_download_request)

    def handle_download_request(self, download_item):
        self.try_start_aria2_daemon()

        with open(os.devnull, "w") as null_file:
            subprocess.Popen(["aria2p", "add", download_item.url().toString()], stdout=null_file)

        self.message_to_emacs.emit("Start download: " + download_item.url().toString())

    def handle_destroy(self):
        self.close_page.emit(self.buffer_widget.url().toString())

        # Load blank page to stop video playing, such as youtube.com.
        self.buffer_widget.open_url("about:blank")

        super.handle_destroy(self)

    def get_key_event_widgets(self):
        # We need send key event to QWebEngineView's focusProxy widget, not QWebEngineView.
        return [self.buffer_widget.focusProxy()]

    def scroll(self, scroll_direction, scroll_type):
        if scroll_type == "page":
            if scroll_direction == "up":
                self.scroll_up_page()
            else:
                self.scroll_down_page()
        else:
            if scroll_direction == "up":
                self.scroll_up()
            else:
                self.scroll_down()

    def handle_input_message(self, result_type, result_content):
        if result_type == "search_text_forward":
            self.buffer_widget._search_text(str(result_content))
        elif result_type == "search_text_backward":
            self.buffer_widget._search_text(str(result_content), True)
        elif result_type == "jump_link":
            self.buffer_widget.jump_to_link(str(result_content).strip())
        elif result_type == "jump_link_new_buffer":
            self.buffer_widget.jump_to_link_new_buffer(str(result_content).strip())
        elif result_type == "jump_link_background_buffer":
            self.buffer_widget.jump_to_link_background_buffer(str(result_content).strip())
        elif result_type == "eval_js_file":
            self.buffer_widget.eval_js_file(str(result_content))

    def cancel_input_message(self, result_type):
        if result_type == "jump_link" or result_type == "jump_link_new_buffer" or result_type == "jump_link_background_buffer":
            self.buffer_widget.cleanup_links()

    def search_text_forward(self):
        self.buffer_widget.search_text_forward()

    def search_text_backward(self):
        self.buffer_widget.search_text_backward()

    def history_backward(self):
        self.buffer_widget.back()

    def history_forward(self):
        self.buffer_widget.forward()

    def clear_all_cookies(self):
        self.buffer_widget.clear_cookies()
        self.message_to_emacs.emit("Cleared all cookies.")

    def action_quit(self):
        self.buffer_widget.search_quit()

    def zoom_out(self):
        self.buffer_widget.zoom_out()

    def zoom_in(self):
        self.buffer_widget.zoom_in()

    def zoom_reset(self):
        self.buffer_widget.zoom_reset()

    def scroll_left(self):
        self.buffer_widget.scroll_left()

    def scroll_right(self):
        self.buffer_widget.scroll_right()

    def try_start_aria2_daemon(self):
        if not is_port_in_use(6800):
            with open(os.devnull, "w") as null_file:
                aria2_args = ["aria2c"]

                aria2_args.append("-d")
                aria2_args.append(os.path.expanduser(str(self.emacs_var_dict["eaf-browser-download-path"])))

                aria2_proxy_host = str(self.emacs_var_dict["eaf-browser-aria2-proxy-host"])
                aria2_proxy_port = str(self.emacs_var_dict["eaf-browser-aria2-proxy-port"])

                if aria2_proxy_host != "" and aria2_proxy_port != "":
                    aria2_args.append("--all-proxy")
                    aria2_args.append("http://{0}:{1}".format(aria2_proxy_host, aria2_proxy_port))

                aria2_args.append("--enable-rpc")
                aria2_args.append("--rpc-listen-all")

                subprocess.Popen(aria2_args, stdout=null_file)

    def open_download_manage_page(self):
        self.try_start_aria2_daemon()
        self.buffer_widget.open_download_manage_page()

    def scroll_up(self):
        self.buffer_widget.scroll_up()

    def scroll_down(self):
        self.buffer_widget.scroll_down()

    def scroll_up_page(self):
        self.buffer_widget.scroll_up_page()

    def scroll_down_page(self):
        self.buffer_widget.scroll_down_page()

    def scroll_to_begin(self):
        self.buffer_widget.scroll_to_begin()

    def scroll_to_bottom(self):
        self.buffer_widget.scroll_to_bottom()

    def refresh_page(self):
        self.buffer_widget.refresh_page()

    def copy_text(self):
        self.buffer_widget.copy_text()
        self.message_to_emacs.emit("Copy selected text.")

    def yank_text(self):
        self.buffer_widget.yank_text()
        self.message_to_emacs.emit("Yank text.")

    def kill_text(self):
        self.buffer_widget.kill_text()
        self.message_to_emacs.emit("Kill text.")

    def undo_action(self):
        self.buffer_widget.undo_action()

    def redo_action(self):
        self.buffer_widget.redo_action()

    def get_url(self):
        return self.buffer_widget.get_url()

    def open_link(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link: ", "jump_link");

    def open_link_new_buffer(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in New Buffer: ", "jump_link_new_buffer");

    def open_link_background_buffer(self):
        self.buffer_widget.get_link_markers()
        self.send_input_message("Open Link in Background Buffer: ", "jump_link_background_buffer");

    def reset_default_zoom(self):
        if hasattr(self, "buffer_widget"):
            self.buffer_widget.setZoomFactor(float(self.emacs_var_dict["eaf-browser-default-zoom"]))

    def edit_focus_text(self):
        text = self.buffer_widget.get_focus_text()
        if text != None:
            self.get_focus_text.emit(self.buffer_id, text)
        else:
            self.message_to_emacs.emit("No active input element.")

    def set_focus_text(self, new_text):
        self.buffer_widget.set_focus_text(new_text)

    def is_focus(self):
        return self.buffer_widget.get_focus_text() != None

    def record_history(self, new_title):
        if self.arguments != "temp_html_file" and new_title != "about:blank" and self.emacs_var_dict["eaf-browser-remember-history"] == "true":
            touch(self.history_log_file_path)
            with open(self.history_log_file_path, "r") as f:
                lines = f.readlines()

            new_url = self.buffer_widget.filter_url(self.buffer_widget.url().toString())
            exists = False
            with open(self.history_log_file_path, "w") as f:
                for line in lines:
                    line_match = re.match(self.history_url_pattern, line)

                    if line_match != None:
                        title = line_match.group(1)
                        url = line_match.group(2)
                    else:
                        title = ""
                        url = line

                    if url == new_url:
                        exists = True
                        if new_title != title:
                            f.write(new_title + " " + new_url + "\n")
                    else:
                        f.write(line)
                if not exists:
                    f.write(new_title + " " + new_url + "\n")

    def adjust_dark_mode(self):
        try:
            if self.emacs_var_dict["eaf-browser-dark-mode"] == "true":
                self.dark_mode()
        except Exception:
            pass

    def new_blank_page(self):
        self.eval_in_emacs.emit('''(eaf-open \"{0}\" \"browser\" \"\" t)'''''.format(self.emacs_var_dict["eaf-browser-blank-page-url"]))

    def clear_history(self):
        if os.path.exists(self.history_log_file_path):
            os.remove(self.history_log_file_path)
            self.message_to_emacs.emit("Cleared browsing history.")
        else:
            self.message_to_emacs.emit("There is no browsing history.")

    def record_close_page(self, url):
        if self.emacs_var_dict["eaf-browser-remember-history"] == "true":
            touch(self.history_close_file_path)
            with open(self.history_close_file_path, "a") as f:
                f.write("{0}\n".format(url))

    def recover_prev_close_page(self):
        if os.path.exists(self.history_close_file_path):
            with open(self.history_close_file_path, "r") as f:
                close_urls = f.readlines()

                if len(close_urls) > 0:
                    # We need use rstrip remove \n char from url record.
                    prev_close_url = close_urls.pop().rstrip()

                    self.open_url_in_new_tab.emit(prev_close_url)
                    open(self.history_close_file_path, "w").writelines(close_urls)

                    self.message_to_emacs.emit("Recovery {0}".format(prev_close_url))
                else:
                    self.message_to_emacs.emit("No page need recovery.")
        else:
            self.message_to_emacs.emit("No page need recovery.")

    def insert_or_do(func):
        def _do(self, *args, **kwargs):
            if self.is_focus():
                self.fake_key_event(self.current_event_string)
            else:
                func(self, *args, **kwargs)
        return _do

    @insert_or_do
    def insert_or_recover_prev_close_page(self):
        self.recover_prev_close_page()

    @insert_or_do
    def insert_or_scroll_up(self):
        self.scroll_up()

    @insert_or_do
    def insert_or_scroll_down(self):
        self.scroll_down()

    @insert_or_do
    def insert_or_scroll_down_page(self):
        self.scroll_down_page()

    @insert_or_do
    def insert_or_scroll_up_page(self):
        self.scroll_up_page()

    @insert_or_do
    def insert_or_scroll_to_begin(self):
        self.scroll_to_begin()

    @insert_or_do
    def insert_or_scroll_to_bottom(self):
        self.scroll_to_bottom()

    @insert_or_do
    def insert_or_open_link(self):
        self.open_link()

    @insert_or_do
    def insert_or_open_link_new_buffer(self):
        self.open_link_new_buffer()

    @insert_or_do
    def insert_or_open_link_background_buffer(self):
        self.open_link_background_buffer()

    @insert_or_do
    def insert_or_history_backward(self):
        self.history_backward()

    @insert_or_do
    def insert_or_history_forward(self):
        self.history_forward()

    @insert_or_do
    def insert_or_scroll_left(self):
        self.scroll_left()

    @insert_or_do
    def insert_or_scroll_right(self):
        self.scroll_right()

    @insert_or_do
    def insert_or_new_blank_page(self):
        self.new_blank_page()

    @insert_or_do
    def insert_or_open_download_manage_page(self):
        self.open_download_manage_page()

    @insert_or_do
    def insert_or_refresh_page(self):
        self.refresh_page()

    @insert_or_do
    def insert_or_close_buffer(self):
        self.request_close_buffer()

    @insert_or_do
    def insert_or_goto_left_tab(self):
        self.goto_left_tab.emit()

    @insert_or_do
    def insert_or_goto_right_tab(self):
        self.goto_right_tab.emit()

    def select_all_or_input_text(self):
        if self.is_focus():
            self.buffer_widget.select_input_text()
        else:
            self.buffer_widget.select_all()

    def clear_focus(self):
        self.buffer_widget.clear_focus()

    def eval_js_file(self):
        self.send_input_message("Eval JS: ", "eval_js_file", "file")

    def dark_mode(self):
        self.buffer_widget.dark_mode()
