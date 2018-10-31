(ns feedparser-clj.test.core
  (:require [clj-time.core :as t]
            [clojure.test :refer :all]
            [feedparser-clj.core :refer :all :reload true])
  (:import [com.rometools.rome.io SyndFeedInput XmlReader]
           java.io.InputStreamReader
           java.net.URL))

(defn load-feed-fixture [name]
  (str (clojure.java.io/resource (format "fixtures/%s" name))))

(deftest parse-test
  (let [pf (parse-feed (load-feed-fixture "gonzih-blog.xml"))]
    (testing :feed
      (is (= (-> pf :author) "gonzih@gmail.com (Max Gonzih)"))
      (is (= (-> pf :categories) []))
      (is (= (-> pf :contributors) []))
      (is (= (-> pf :entry-links) []))
      (is (= (-> pf :image) nil))
      (is (= (-> pf :copyright) "This work is licensed under a Creative Commons Attribution 4.0 International License."))
      (is (= (-> pf :description) "Recent content on Max Gonzih"))
      (is (= (-> pf :encoding) nil))
      (is (= (-> pf :feed-type) "rss_2.0"))
      (is (= (-> pf :language) "en-us"))
      (is (= (-> pf :link) "http://blog.gonzih.me/index.xml"))
      (is (= (-> pf :published-date) (t/date-time 2015 12 11)))
      (is (= (-> pf :title) "Max Gonzih"))
      (is (= (-> pf :uri) nil)))

    (testing :entry
      (is (= (-> pf :entries count) 15))
      (let [entry (-> pf :entries first)]
        (is (= (:authors entry) []))
        (is (= (:categories entry) []))
        (is (= (:contributors entry) []))
        (is (= (:enclosures entry) []))
        (is (= (:contents entry) []))
        (is (= "text/html" (:type (:description entry))))
        (is (re-find #"Collection of tweaks that I gathered after installing Arch.*" (:value (:description entry))))
        (is (= (:author entry) "gonzih@gmail.com (Max Gonzih)"))
        (is (= (:link entry) "http://blog.gonzih.me/blog/2015/12/11/arch-linux-on-lenovo-ideapad-y700-15/"))
        (is (= (:published-date entry) (t/date-time 2015 12 11)))
        (is (= (:title entry) "Arch Linux on Lenovo IdeaPad Y700 15\""))
        (is (= (:updated-date entry) nil))
        (is (= (:uri entry) "http://blog.gonzih.me/blog/2015/12/11/arch-linux-on-lenovo-ideapad-y700-15/"))))))

(deftest parse-itunes-podcast-feed-test
  (let [pf (parse-feed (load-feed-fixture "fish.xml"))]
    (testing :feed
      (is (= (-> pf :author) "Audioboom"))
      (is (= (-> pf :categories) []))
      (is (= (-> pf :contributors) []))
      (is (= (-> pf :entry-links) []))
      (is (= (-> pf :image :url) "https://images.theabcdn.com/i/24025650.jpg"))
      (is (= (-> pf :copyright) nil))
      (is (re-find #"A podcast from the QI offices.*" (-> pf :description)))
      (is (= (-> pf :encoding) nil))
      (is (= (-> pf :feed-type) "rss_2.0"))
      (is (= (-> pf :language) "en-us"))
      (is (= (-> pf :link) "https://audioboom.com/channel/nosuchthingasafish"))
      (is (= (-> pf :published-date) (t/date-time 2018 10 26 17 0 5)))
      (is (= (-> pf :title) "No Such Thing As A Fish"))
      (is (= (-> pf :uri) nil))
      (is (= (-> pf :block) false))
      (is (= (-> pf :explicit) false))
      (is (= (-> pf :subtitle) nil))
      (is (= (-> pf :keywords) nil))
      (is (re-find #"A podcast from the QI offices.*" (-> pf :summary))))

    (testing :entry
      (is (= (-> pf :entries count) 157))
      (let [entry (-> pf :entries first)]
        (is (= (:authors entry) []))
        (is (= (:categories entry) []))
        (is (= (:contributors entry) []))
        (is (= (-> entry :enclosures first :url) "https://audioboom.com/posts/7060991.mp3?modified=1540570766&source=rss&stitched=1"))
        (is (= (re-find #".*Dan, James, Anna, Andy and special guest.*" (-> entry :contents first :value))))
        (is (= "text/html" (:type (:description entry))))
        (is (re-find #".*Dan, James, Anna, Andy and special guest.*" (:value (:description entry))))
        (is (= (:author entry) "Audioboom"))
        (is (= (:link entry) "https://audioboom.com/posts/7060991"))
        (is (= (:published-date entry) (t/date-time 2018 10 26 17)))
        (is (= (:title entry) "Episode 240: No Such Thing As An Easy Tweet"))
        (is (= (:updated-date entry) nil))
        (is (= (:uri entry) "tag:audioboom.com,2018-10-26:/posts/7060991"))
        (is (= (:block entry) false))
        (is (= (:closed-captioned entry) false))
        (is (= (:keywords entry) nil))
        (is (= (:season entry) nil))
        (is (re-find #".*Dan, James, Anna, Andy and special guest.*" (:summary entry)))
        (is (re-find #".*Dan, James, Anna, Andy and special guest.*" (:subtitle entry)))
        (is (= (:episode-type entry) "full"))
        (is (= (:order entry) nil))
        (is (= (-> entry :image :url) "https://images.theabcdn.com/i/33781874.jpg"))
        (is (= (:episode entry) nil))))))
