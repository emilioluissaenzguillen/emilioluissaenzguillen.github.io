---
layout: archive
title: "Publications"
permalink: /publications/
author_profile: true
---

{% if site.author.googlescholar %}
  <div class="wordwrap">You can also find my articles on <a href="{{site.author.googlescholar}}">my Google Scholar profile</a>.</div>
{% endif %}

{% include base_path %}

<div class="publications-list">
  {% for post in site.publications reversed %}
    <article class="archive-item" id="{{ post.url | slugify }}">
      <h3>{{ post.title }}</h3>
      <p>{{ post.citation | markdownify }}</p>
    </article>
    <hr>
  {% endfor %}
</div>
