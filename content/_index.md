---
# Leave the homepage title empty to use the site title
title:
date: 2022-10-24
type: landing

sections:
  - block: hero
    content:
      title: |
        Who We Are
      image:
        filename: 1.jpg
      text: |
        <br>
        
        **The Special Forces 209 (SF209)** at [Taipei Tech](https://www-en.ntut.edu.tw/) is an academic unit dedicated to the development and application of advanced organizational research methods. Our work focuses on the intersection of statistical theory and survey methodology, with a specific specialization in complex survey data analysis. We aim to provide researchers and practitioners with robust quantitative frameworks and mathematically sound insights to address methodological challenges in organizational science. Follow us on [social media](https://linktr.ee/sf209.taipeitech) for the latest updates on our research, events, and insights! 
        
#        {{< audio src="walkout song.mp3" >}}
  
  - block: collection
    content:
      title: Latest News
      subtitle:
      text:
      count: 5
      filters:
        author: ''
        category: ''
        exclude_featured: false
        publication_type: ''
        tag: ''
      offset: 0
      order: desc
      page_type: post
    design:
      view: card
      columns: '1'
  
  - block: markdown
    content:
      title:
      subtitle: ''
      text:
    design:
      columns: '1'
      background:
        image: 
          filename: coders.jpg
          filters:
            brightness: 1
          parallax: false
          position: center
          size: cover
          text_color_light: true
      spacing:
        padding: ['20px', '0', '20px', '0']
      css_class: fullscreen

  - block: collection
    content:
      title: Latest Preprints
      text: ""
      count: 5
      filters:
        folders:
          - publication
        publication_type: 'article'
    design:
      view: citation
      columns: '1'

  - block: markdown
    content:
      title:
      subtitle:
      text: |
        {{% cta cta_link="./people/" cta_text="Meet the team →" %}}
    design:
      columns: '1'
---
