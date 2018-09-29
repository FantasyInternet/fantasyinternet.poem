(global $~data (mut i32) (i32.const 0))
(global $~width (mut i32) (i32.const 0))
(global $~height (mut i32) (i32.const 0))
(global $~tile_width (mut i32) (i32.const 0))
(global $~tile_height (mut i32) (i32.const 0))
(global $screen (mut i32) (i32.const 0))
(global $~blending_mode (mut i32) (i32.const 0))
(global $~opacity (mut i32) (i32.const 255))
(global $font (mut i32) (i32.const 0))
(global $~txtX (mut i32) (i32.const 0))
(global $~txtY (mut i32) (i32.const 0))

(func $set_blending_mode (param $mode i32) (result i32)
  (set_global $~blending_mode (call $-i32_u (get_local $mode)))
  (get_global $~blending_mode)
)
(func $set_opacity (param $opacity i32) (result i32)
  (set_global $~opacity (call $-i32_u (get_local $opacity)))
  (get_global $~opacity)
)

(func $create_image (param $width i32) (param $height i32) (result i32)
  (local $out i32)
  (if (i32.eqz (get_global $~data))(then
    (set_global $~data (call $-new_value   (i32.const 3) (i32.const 4)))
    (call $-write32 (get_global $~data)    (i32.const 0) (i32.const 0x61746164))
    (set_global $~width (call $-new_value  (i32.const 3) (i32.const 5)))
    (call $-write32 (get_global $~width)   (i32.const 0) (i32.const 0x74646977))
    (call $-write8  (get_global $~width)   (i32.const 4) (i32.const 0x68))
    (set_global $~height (call $-new_value (i32.const 3) (i32.const 6)))
    (call $-write32 (get_global $~height)  (i32.const 0) (i32.const 0x67696568))
    (call $-write16 (get_global $~height)  (i32.const 4) (i32.const 0x7468))
    (set_global $~tile_width (call $-new_value  (i32.const 3) (i32.const 10)))
    (call $-write32 (get_global $~tile_width)   (i32.const 0) (i32.const 0x656c6974))
    (call $-write32 (get_global $~tile_width)   (i32.const 4) (i32.const 0x6469775f))
    (call $-write16 (get_global $~tile_width)   (i32.const 8) (i32.const 0x6874))
    (set_global $~tile_height (call $-new_value (i32.const 3) (i32.const 11)))
    (call $-write32 (get_global $~tile_height)  (i32.const 0) (i32.const 0x656c6974))
    (call $-write32 (get_global $~tile_height)  (i32.const 4) (i32.const 0x6965685f))
    (call $-write16 (get_global $~tile_height)  (i32.const 8) (i32.const 0x6867))
    (call $-write8  (get_global $~tile_height)  (i32.const 10) (i32.const 0x74))
  ))
  (set_local $out (call $-new_value (i32.const 5) (i32.const 0)))
  (call $-set_to_obj
    (get_local $out)
    (get_global $~data)
    (call $-new_value
      (i32.const 6)
      (i32.mul
        (i32.const 4)
        (i32.mul
          (call $-i32_u (get_local $width) )
          (call $-i32_u (get_local $height) )
        )
      )
    )
  )
  (call $-set_to_obj (get_local $out) (get_global $~width) (get_local $width))
  (call $-set_to_obj (get_local $out) (get_global $~height) (get_local $height))
  (get_local $out)
)

(func $rgb (param $r i32) (param $g i32) (param $b i32) (param $a i32) (result i32)
  (local $out i32)
  (set_local $r (call $-i32_s (get_local $r)))
  (if (i32.eqz (get_local $g))(then
    (set_local $g (get_local $r))
    (set_local $b (get_local $r))
    (set_local $a (i32.const 255))
  )(else
    (if (i32.eqz (get_local $b))(then
      (set_local $a (call $-i32_s (get_local $g)))
      (set_local $g (get_local $r))
      (set_local $b (get_local $r))
    )(else
      (if (i32.eqz (get_local $a))(then
        (set_local $g (call $-i32_s (get_local $g)))
        (set_local $b (call $-i32_s (get_local $b)))
        (set_local $a (i32.const 255))
      )(else
        (set_local $g (call $-i32_s (get_local $g)))
        (set_local $b (call $-i32_s (get_local $b)))
        (set_local $a (call $-i32_s (get_local $a)))
      ))
    ))
  ))
  (if (i32.lt_s (get_local $r) (i32.const 0))(then
    (set_local $r (i32.const 0))
  ))
  (if (i32.lt_s (get_local $g) (i32.const 0))(then
    (set_local $g (i32.const 0))
  ))
  (if (i32.lt_s (get_local $b) (i32.const 0))(then
    (set_local $b (i32.const 0))
  ))
  (if (i32.lt_s (get_local $a) (i32.const 0))(then
    (set_local $a (i32.const 0))
  ))
  (if (i32.gt_s (get_local $r) (i32.const 255))(then
    (set_local $r (i32.const 255))
  ))
  (if (i32.gt_s (get_local $g) (i32.const 255))(then
    (set_local $g (i32.const 255))
  ))
  (if (i32.gt_s (get_local $b) (i32.const 255))(then
    (set_local $b (i32.const 255))
  ))
  (if (i32.gt_s (get_local $a) (i32.const 255))(then
    (set_local $a (i32.const 255))
  ))
  (set_local $out (call $-new_value (i32.const 6) (i32.const 4)))
  (call $-write8 (get_local $out) (i32.const 0) (get_local $r))
  (call $-write8 (get_local $out) (i32.const 1) (get_local $g))
  (call $-write8 (get_local $out) (i32.const 2) (get_local $b))
  (call $-write8 (get_local $out) (i32.const 3) (get_local $a))
  (get_local $out)
)

(func $pset (param $img i32) (param $x i32) (param $y i32) (param $c i32) (result i32)
  (local $w i32)
  (local $h i32)
  (if (i32.eq (call $-datatype (get_local $img)) (i32.const 2))(then
    (set_local $c (get_local $y))
    (set_local $y (get_local $x))
    (set_local $x (get_local $img))
    (set_local $img (get_global $screen))
  ))
  (set_local $x (call $-i32_u (get_local $x)))
  (set_local $y (call $-i32_u (get_local $y)))
  (set_local $c (call $-read32 (get_local $c) (i32.const 0)))
  (set_local $w (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~width))))
  (set_local $h (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~height))))
  (set_local $img (call $-get_from_obj (get_local $img) (get_global $~data)))
  (if
    (i32.and
      (i32.lt_u (get_local $x) (get_local $w))
      (i32.lt_u (get_local $y) (get_local $h))
    )
  (then
    (call $~pset
      (i32.add 
        (call $-offset (get_local $img))
        (i32.mul
          (i32.add
            (i32.mul
              (get_local $y)
              (get_local $w)
            )
            (get_local $x)
          )
          (i32.const 4)
        )
      )
      (get_local $c)
    )
  ))
  (i32.const 0)
)
(func $~pset (param $addr i32) (param $c i32)
  (local $br i32)
  (local $bg i32)
  (local $bb i32)
  (local $ba i32)
  (local $fr i32)
  (local $fg i32)
  (local $fb i32)
  (local $fa i32)
  ;; apply opacity
  (if (i32.lt_u (get_global $~opacity) (i32.const 0xff)) (then
    (set_local $fa (i32.div_u (get_local $c) (i32.const 0x01000000)))
    (set_local $fa (i32.div_u (i32.mul (get_local $fa) (get_global $~opacity)) (i32.const 0xff)))
    (set_local $c (i32.add
      (i32.and (get_local $c)  (i32.const 0x00ffffff))
      (i32.mul (get_local $fa) (i32.const 0x01000000))
    ))
  ))
  ;; replace
  (if (i32.eq (get_global $~blending_mode) (i32.const 0))(then
    (i32.store (get_local $addr) (get_local $c))
    (br 1)
  ))
  ;; alpha blending
  (if (i32.eq (get_global $~blending_mode) (i32.const 1))(then
    (if (i32.gt_u (get_local $c) (i32.const 0x00ffffff))(then
      (if (i32.ge_u (get_local $c) (i32.const 0xff000000))(then
        (i32.store (get_local $addr) (get_local $c))
      )(else
        (set_local $fr (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.rotr (get_local $c) (i32.const 8)))
        (set_local $fg (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.rotr (get_local $c) (i32.const 8)))
        (set_local $fb (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.rotr (get_local $c) (i32.const 8)))
        (set_local $fa (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.load (get_local $addr)))
        (set_local $br (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.rotr (get_local $c) (i32.const 8)))
        (set_local $bg (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.rotr (get_local $c) (i32.const 8)))
        (set_local $bb (i32.and (get_local $c) (i32.const 0xff)))
        (set_local $c (i32.rotr (get_local $c) (i32.const 8)))
        (set_local $ba (i32.and (get_local $c) (i32.const 0xff)))

        (if (i32.ne (get_local $br) (get_local $fr))(then
          (set_local $br (i32.add
            (get_local $br)
            (i32.div_s
              (i32.mul
                (i32.sub
                  (get_local $fr)
                  (get_local $br)
                )
                (get_local $fa)
              )
              (i32.const 0xff)
            )
          ))
          (i32.store8 (i32.add (get_local $addr) (i32.const 0)) (get_local $br))
        ))
        (if (i32.ne (get_local $bg) (get_local $fg))(then
          (set_local $bg (i32.add
            (get_local $bg)
            (i32.div_s
              (i32.mul
                (i32.sub
                  (get_local $fg)
                  (get_local $bg)
                )
                (get_local $fa)
              )
              (i32.const 0xff)
            )
          ))
          (i32.store8 (i32.add (get_local $addr) (i32.const 1)) (get_local $bg))
        ))
        (if (i32.ne (get_local $bb) (get_local $fb))(then
          (set_local $bb (i32.add
            (get_local $bb)
            (i32.div_s
              (i32.mul
                (i32.sub
                  (get_local $fb)
                  (get_local $bb)
                )
                (get_local $fa)
              )
              (i32.const 0xff)
            )
          ))
          (i32.store8 (i32.add (get_local $addr) (i32.const 2)) (get_local $bb))
        ))
        (if (i32.lt_u (get_local $ba) (i32.const 0xff))(then
          (set_local $ba (i32.add
            (get_local $ba)
            (i32.div_u
              (i32.mul
                (get_local $fa)
                (i32.sub
                  (i32.const 0xff)
                  (get_local $ba)
                )
              )
              (i32.const 0xff)
            )
          ))
          (i32.store8 (i32.add (get_local $addr) (i32.const 3)) (get_local $ba))
        ))
      ))
    ))
    (br 1)
  ))
  ;; alpha lock
  (if (i32.eq (get_global $~blending_mode) (i32.const 2))(then
    (set_local $c (i32.add
      (i32.and (get_local $c) (i32.const 0x00ffffff))
      (i32.and (i32.load (get_local $addr)) (i32.const 0xff000000))
    ))
    (i32.store (get_local $addr) (get_local $c))
    (br 1)
  ))
)

(func $pget (param $img i32) (param $x i32) (param $y i32) (result i32)
  (local $c i32)
  (if (i32.eq (call $-datatype (get_local $img)) (i32.const 2))(then
    (set_local $y (get_local $x))
    (set_local $x (get_local $img))
    (set_local $img (get_global $screen))
  ))
  (set_local $x (call $-i32_u (get_local $x)))
  (set_local $y (call $-i32_u (get_local $y)))
  (set_local $c (call $-new_value (i32.const 6) (i32.const 4)))
  (call $-write32 (get_local $c) (i32.const 0) (call $~pget (get_local $img) (get_local $x) (get_local $y)))
  (get_local $c)
)
(func $~pget (param $img i32) (param $x i32) (param $y i32) (result i32)
  (local $w i32)
  (local $h i32)
  (local $c i32)
  (set_local $w (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~width))))
  (set_local $h (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~height))))
  (set_local $img (call $-get_from_obj (get_local $img) (get_global $~data)))
  (if
    (i32.and
      (i32.lt_u (get_local $x) (get_local $w))
      (i32.lt_u (get_local $y) (get_local $h))
    )
  (then
    (set_local $c
      (call $-read32
        (get_local $img)
        (i32.mul
          (i32.add
            (i32.mul
              (get_local $y)
              (get_local $w)
            )
            (get_local $x)
          )
          (i32.const 4)
        )
      )
    )
  ))
  (get_local $c)
)

(func $rect (param $img i32) (param $x i32) (param $y i32) (param $w i32) (param $h i32) (param $c i32) (result i32)
  (i32.const 0)
  (if (i32.eq (call $-datatype (get_local $img)) (i32.const 2))(then
    (set_local $c (get_local $h))
    (set_local $h (get_local $w))
    (set_local $w (get_local $y))
    (set_local $y (get_local $x))
    (set_local $x (get_local $img))
    (set_local $img (get_global $screen))
  ))
  ;; read params
  (set_local $x (call $-i32_s (get_local $x)))
  (set_local $y (call $-i32_s (get_local $y)))
  (set_local $w (call $-i32_s (get_local $w)))
  (set_local $h (call $-i32_s (get_local $h)))
  (set_local $c (call $-read32 (get_local $c) (i32.const 0)))
  (call $~rect (get_local $img) (get_local $x) (get_local $y) (get_local $w) (get_local $h) (get_local $c))
)
(func $~rect (param $img i32) (param $x i32) (param $y i32) (param $w i32) (param $h i32) (param $c i32)
  (local $i i32)
  (local $j i32)
  (local $imgOffset i32)
  (local $imgWidth i32)
  (local $imgHeight i32)

  ;; unpack image
  (set_local $imgWidth  (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~width ))))
  (set_local $imgHeight (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~height))))
  (set_local $img       (call $-get_from_obj (get_local $img) (get_global $~data)))
  (set_local $imgOffset (call $-offset (get_local $img)))
  
  ;; clamp rect
  (br_if 0 (i32.ge_s (get_local $x) (get_local $imgWidth)))
  (br_if 0 (i32.ge_s (get_local $y) (get_local $imgHeight)))
  (br_if 0 (i32.lt_s (i32.add (get_local $x) (get_local $w)) (i32.const 0)))
  (br_if 0 (i32.lt_s (i32.add (get_local $y) (get_local $h)) (i32.const 0)))
  (if (i32.lt_s (get_local $x) (i32.const 0)) (then
    (set_local $w (i32.add (get_local $w) (get_local $x)))
    (set_local $x (i32.const 0))
  ))
  (if (i32.lt_s (get_local $y) (i32.const 0)) (then
    (set_local $h (i32.add (get_local $h) (get_local $y)))
    (set_local $y (i32.const 0))
  ))
  (if (i32.gt_s (i32.add (get_local $x) (get_local $w)) (get_local $imgWidth)) (then
    (set_local $w (i32.sub (get_local $imgWidth) (get_local $x)))))
  (if (i32.gt_s (i32.add (get_local $y) (get_local $h)) (get_local $imgHeight)) (then
    (set_local $h (i32.sub (get_local $imgHeight) (get_local $y)))))
  (set_local $i (i32.mul (i32.const 4) (i32.add (get_local $x) (i32.mul (get_local $y) (get_local $imgWidth)))))

  ;; starting point
  (set_local $imgOffset (i32.add (get_local $imgOffset)
    (i32.mul (i32.const 4) (i32.add (get_local $x) (i32.mul (get_local $y) (get_local $imgWidth))))
  ))
  ;; loop
  (block (set_local $i (get_local $h)) (loop
    (br_if 1 (i32.eqz (get_local $i) ))
    (block (set_local $j (get_local $w)) (loop
      (br_if 1 (i32.eqz (get_local $j) ))

      (call $~pset (get_local $imgOffset) (get_local $c))
      (set_local $imgOffset (i32.add (get_local $imgOffset) (i32.const 4)))

      (set_local $j (i32.sub (get_local $j) (i32.const 1)))
      (br 0)
    ))
    (set_local $imgOffset (i32.add (get_local $imgOffset) (i32.mul (i32.const 4)
      (i32.sub (get_local $imgWidth) (get_local $w))
    )))

    (set_local $i (i32.sub (get_local $i) (i32.const 1)))
    (br 0)
  ))
)
(func $line (param $img i32) (param $x1 i32) (param $y1 i32) (param $x2 i32) (param $y2 i32) (param $c i32) (result i32)
  (local $dx i32)
  (local $dy i32)
  (local $D i32)
  (local $x i32)
  (local $y i32)
  (local $xi i32)
  (local $yi i32)
  (local $i i32)
  (local $w i32)
  (local $h i32)
  (i32.const 0)
  ;; read params
  (if (i32.eq (call $-datatype (get_local $img)) (i32.const 2))(then
    (set_local $c (get_local $y2))
    (set_local $y2 (get_local $x2))
    (set_local $x2 (get_local $y1))
    (set_local $y1 (get_local $x1))
    (set_local $x1 (get_local $img))
    (set_local $img (get_global $screen))
  ))
  (set_local $x1 (call $-i32_s (get_local $x1)))
  (set_local $y1 (call $-i32_s (get_local $y1)))
  (set_local $x2 (call $-i32_s (get_local $x2)))
  (set_local $y2 (call $-i32_s (get_local $y2)))
  (set_local $c (call $-read32 (get_local $c) (i32.const 0)))

  (set_local $w (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~width))))
  (set_local $h (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~height))))
  (set_local $img (call $-get_from_obj (get_local $img) (get_global $~data)))

  ;; Bresenham's
  (set_local $dx (i32.sub (get_local $x2) (get_local $x1)))
  (set_local $dy (i32.sub (get_local $y2) (get_local $y1)))
  (if (i32.lt_s (get_local $dx) (i32.const 0))(then
    (set_local $xi (i32.const -1))
    (set_local $dx (i32.mul (get_local $dx) (i32.const -1)))
  )(else
    (set_local $xi (i32.const 1))
  ))
  (if (i32.lt_s (get_local $dy) (i32.const 0))(then
    (set_local $yi (i32.const -1))
    (set_local $dy (i32.mul (get_local $dy) (i32.const -1)))
  )(else
    (set_local $yi (i32.const 1))
  ))
  (set_local $x (get_local $x1))
  (set_local $y (get_local $y1))
  (set_local $x2 (i32.add (get_local $x2) (get_local $xi)))
  (set_local $y2 (i32.add (get_local $y2) (get_local $yi)))

  (if (i32.gt_u (get_local $dx) (get_local $dy))(then
    (set_local $D  (i32.sub (i32.mul (get_local $dy) (i32.const 2)) (get_local $dx)))
    (block(loop (br_if 1 (i32.eq (get_local $x) (get_local $x2)))
      (if (i32.and
        (i32.lt_u (get_local $x) (get_local $w))
        (i32.lt_u (get_local $y) (get_local $h))
      )(then
        (set_local $i (i32.mul
          (i32.add
            (i32.mul
              (get_local $y)
              (get_local $w)
            )
            (get_local $x)
          )
          (i32.const 4)
        ))
        (call $~pset
          (i32.add
            (call $-offset (get_local $img))
            (get_local $i)
          )
          (get_local $c)
        )
      ))
      (if (i32.gt_s (get_local $D) (i32.const 0))(then
        (set_local $y (i32.add (get_local $y) (get_local $yi)))
        (set_local $D (i32.sub
          (get_local $D)
          (i32.mul
            (i32.const 2)
            (get_local $dx)
          )
        ))
      ))
      (set_local $D (i32.add
        (get_local $D)
        (i32.mul
          (i32.const 2)
          (get_local $dy)
        )
      ))
      (set_local $x (i32.add (get_local $x) (get_local $xi)))
    (br 0)))
  )(else
    (set_local $D  (i32.sub (i32.mul (get_local $dx) (i32.const 2)) (get_local $dy)))
    (block(loop (br_if 1 (i32.eq (get_local $y) (get_local $y2)))
      (if (i32.and
        (i32.lt_u (get_local $x) (get_local $w))
        (i32.lt_u (get_local $y) (get_local $h))
      )(then
        (set_local $i (i32.mul
          (i32.add
            (i32.mul
              (get_local $y)
              (get_local $w)
            )
            (get_local $x)
          )
          (i32.const 4)
        ))
        (call $~pset
          (i32.add
            (call $-offset (get_local $img))
            (get_local $i)
          )
          (get_local $c)
        )
      ))
      (if (i32.gt_s (get_local $D) (i32.const 0))(then
        (set_local $x (i32.add (get_local $x) (get_local $xi)))
        (set_local $D (i32.sub
          (get_local $D)
          (i32.mul
            (i32.const 2)
            (get_local $dy)
          )
        ))
      ))
      (set_local $D (i32.add
        (get_local $D)
        (i32.mul
          (i32.const 2)
          (get_local $dx)
        )
      ))
      (set_local $y (i32.add (get_local $y) (get_local $yi)))
    (br 0)))
  ))
)

(func $draw_image (param $simg i32) (param $sx i32) (param $sy i32) (param $dimg i32) (param $dx i32) (param $dy i32) (param $w i32) (param $h i32) (result i32)
  (set_local $sx (call $-i32_s (get_local $sx)))
  (set_local $sy (call $-i32_s (get_local $sy)))
  (set_local $dx (call $-i32_s (get_local $dx)))
  (set_local $dy (call $-i32_s (get_local $dy)))
  (set_local $w  (call $-i32_s (get_local $w)))
  (set_local $h  (call $-i32_s (get_local $h)))
  (call $~draw_image (get_local $simg) (get_local $sx) (get_local $sy) (get_local $dimg) (get_local $dx) (get_local $dy) (get_local $w) (get_local $h))
  (i32.const 0)
)
(func $~draw_image (param $simg i32) (param $sx i32) (param $sy i32) (param $dimg i32) (param $dx i32) (param $dy i32) (param $w i32) (param $h i32)
  (local $x i32)
  (local $y i32)
  (local $sw i32)
  (local $sh i32)
  (local $soff i32)
  (local $dw i32)
  (local $dh i32)
  (local $doff i32)
  (if (i32.eqz (get_local $simg)) (return))
  (if (i32.eqz (get_local $dimg)) (return))

  ;; unpack source image
  (set_local $sw (call $-i32_u  (call $-get_from_obj  (get_local $simg) (get_global $~width ))))
  (set_local $sh (call $-i32_u  (call $-get_from_obj  (get_local $simg) (get_global $~height))))
  (set_local $simg              (call $-get_from_obj  (get_local $simg) (get_global $~data)))
  (set_local $soff              (call $-offset        (get_local $simg)))

  ;; unpack dest image
  (set_local $dw (call $-i32_u  (call $-get_from_obj  (get_local $dimg) (get_global $~width ))))
  (set_local $dh (call $-i32_u  (call $-get_from_obj  (get_local $dimg) (get_global $~height))))
  (set_local $dimg              (call $-get_from_obj  (get_local $dimg) (get_global $~data)))
  (set_local $doff              (call $-offset        (get_local $dimg)))

  ;; clamp source rect
  (br_if 0 (i32.ge_s (get_local $sx) (get_local $sw)))
  (br_if 0 (i32.ge_s (get_local $sy) (get_local $sh)))
  (br_if 0 (i32.lt_s (i32.add (get_local $sx) (get_local $w)) (i32.const 0)))
  (br_if 0 (i32.lt_s (i32.add (get_local $sy) (get_local $h)) (i32.const 0)))
  (if (i32.lt_s (get_local $sx) (i32.const 0)) (then
    (set_local $dx (i32.sub (get_local $dx) (get_local $sx)))
    (set_local $w (i32.add (get_local $w) (get_local $sx)))
    (set_local $sx (i32.const 0))
  ))
  (if (i32.lt_s (get_local $sy) (i32.const 0)) (then
    (set_local $dy (i32.sub (get_local $dy) (get_local $sy)))
    (set_local $h (i32.add (get_local $h) (get_local $sy)))
    (set_local $sy (i32.const 0))
  ))
  (if (i32.gt_s (i32.add (get_local $sx) (get_local $w)) (get_local $sw)) (then
    (set_local $w (i32.sub (get_local $sw) (get_local $sx)))))
  (if (i32.gt_s (i32.add (get_local $sy) (get_local $h)) (get_local $sh)) (then
    (set_local $h (i32.sub (get_local $sh) (get_local $sy)))))

  ;; clamp dest rect
  (br_if 0 (i32.ge_s (get_local $dx) (get_local $dw)))
  (br_if 0 (i32.ge_s (get_local $dy) (get_local $dh)))
  (br_if 0 (i32.lt_s (i32.add (get_local $dx) (get_local $w)) (i32.const 0)))
  (br_if 0 (i32.lt_s (i32.add (get_local $dy) (get_local $h)) (i32.const 0)))
  (if (i32.lt_s (get_local $dx) (i32.const 0)) (then
    (set_local $sx (i32.sub (get_local $sx) (get_local $dx)))
    (set_local $w (i32.add (get_local $w) (get_local $dx)))
    (set_local $dx (i32.const 0))
  ))
  (if (i32.lt_s (get_local $dy) (i32.const 0)) (then
    (set_local $sy (i32.sub (get_local $sy) (get_local $dy)))
    (set_local $h (i32.add (get_local $h) (get_local $dy)))
    (set_local $dy (i32.const 0))
  ))
  (if (i32.gt_s (i32.add (get_local $dx) (get_local $w)) (get_local $dw)) (then
    (set_local $w (i32.sub (get_local $dw) (get_local $dx)))))
  (if (i32.gt_s (i32.add (get_local $dy) (get_local $h)) (get_local $dh)) (then
    (set_local $h (i32.sub (get_local $dh) (get_local $dy)))))

  ;; starting point
  (set_local $soff (i32.add (get_local $soff)
    (i32.mul (i32.const 4) (i32.add (get_local $sx) (i32.mul (get_local $sy) (get_local $sw))))
  ))
  (set_local $doff (i32.add (get_local $doff)
    (i32.mul (i32.const 4) (i32.add (get_local $dx) (i32.mul (get_local $dy) (get_local $dw))))
  ))
  ;; loop
  (block (set_local $y (get_local $h)) (loop
    (br_if 1 (i32.eqz (get_local $y) ))
    (block (set_local $x (get_local $w)) (loop
      (br_if 1 (i32.eqz (get_local $x) ))

      (call $~pset (get_local $doff) (i32.load (get_local $soff)))
      (set_local $soff (i32.add (get_local $soff) (i32.const 4)))
      (set_local $doff (i32.add (get_local $doff) (i32.const 4)))

      (set_local $x (i32.sub (get_local $x) (i32.const 1)))
      (br 0)
    ))
    (set_local $soff (i32.add (get_local $soff) (i32.mul (i32.const 4)
      (i32.sub (get_local $sw) (get_local $w))
    )))
    (set_local $doff (i32.add (get_local $doff) (i32.mul (i32.const 4)
      (i32.sub (get_local $dw) (get_local $w))
    )))

    (set_local $y (i32.sub (get_local $y) (i32.const 1)))
    (br 0)
  ))
)

(func $set_font (param $img i32) (param $tile_width i32) (param $tile_height i32) (result i32)
  (call $-set_to_obj (get_local $img) (get_global $~tile_width) (get_local $tile_width))
  (call $-set_to_obj (get_local $img) (get_global $~tile_height) (get_local $tile_height))
  (set_global $font (get_local $img))
  (get_global $font)
)

(func $set_cursor (param $x i32) (param $y i32) (result i32)
  (set_global $~txtX (call $-i32_u (get_local $x)))
  (set_global $~txtY (call $-i32_u (get_local $y)))
  (i32.const 0)
)
(func $get_cursor_x (result i32)
  (call $-integer_u (get_global $~txtX))
)
(func $get_cursor_y (result i32)
  (call $-integer_u (get_global $~txtY))
)

(func $draw_text (param $text i32) (param $img i32) (result i32)
  (local $tw i32)
  (local $th i32)
  (local $cols i32)
  (local $w i32)
  (local $h i32)
  (local $pos i32)
  (local $len i32)
  (local $char i32)
  (local $x i32)
  (local $y i32)
  (i32.const 0)
  (if (i32.eqz (get_local $img))(then
    (set_local $img (get_global $screen))
  ))
  (if (i32.eqz (get_global $font))(return (i32.const 0)))
  (if (i32.eqz (get_local $img))(return (i32.const 0)))
  (set_local $tw (call $-i32_u (call $-get_from_obj (get_global $font) (get_global $~tile_width))))
  (set_local $th (call $-i32_u (call $-get_from_obj (get_global $font) (get_global $~tile_height))))
  (set_local $cols (i32.div_u (call $-i32_u (call $-get_from_obj (get_global $font) (get_global $~width))) (get_local $tw)))
  (set_local $w  (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~width))))
  (set_local $h  (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $~height))))
  (set_local $pos (call $-offset (get_local $text)))
  (set_local $len (call $-len (get_local $text)))
  (block(loop (br_if 1 (i32.eqz (get_local $len)))
    (set_local $char (call $-char_code (get_local $pos)))
    (if (i32.eq (get_local $char) (i32.const 9))(then
      (set_global $~txtX
        (i32.mul
          (i32.div_u
            (i32.add
              (get_global $~txtX)
              (i32.mul
                (get_local $tw)
                (i32.const 8)
              )
            )
            (i32.mul
              (get_local $tw)
              (i32.const 8)
            )
          )
          (i32.mul
            (get_local $tw)
            (i32.const 8)
          )
        )
      )
    ))
    (if (i32.eq (get_local $char) (i32.const 10))(then
      (set_global $~txtX (i32.const 0))
      (set_global $~txtY (i32.add (get_global $~txtY) (get_local $th)))
    ))
    (if (i32.ge_u (get_local $char) (i32.const 32))(then
      (set_local $char (i32.sub (get_local $char) (i32.const 32)))
      (set_local $x (i32.mul (i32.rem_u (get_local $char) (get_local $cols)) (get_local $tw)))
      (set_local $y (i32.mul (i32.div_u (get_local $char) (get_local $cols)) (get_local $th)))
      (call $~draw_image
        (get_global $font)
        (get_local  $x)
        (get_local  $y)
        (get_local  $img)
        (get_global $~txtX)
        (get_global $~txtY)
        (get_local  $tw)
        (get_local  $th)
      )
      (set_global $~txtX (i32.add (get_global $~txtX) (get_local $tw)))
      (set_local $char (i32.add (get_local $char) (i32.const 32)))
    ))
    (if (i32.lt_s (i32.sub (get_local $w) (get_global $~txtX)) (get_local $tw))(then
      (set_global $~txtX (i32.const 0))
      (set_global $~txtY (i32.add (get_global $~txtY) (get_local $th)))
      (if (i32.lt_s (i32.sub (get_local $h) (get_global $~txtY)) (get_local $th))(then
        (set_local $x (get_global $~blending_mode))
        (set_local $y (get_global $~opacity))
        (set_global $~blending_mode (i32.const 0))
        (set_global $~opacity (i32.const 255))
        (call $~draw_image
          (get_local $img)
          (i32.const 0)
          (i32.sub
            (get_global $~txtY)
            (i32.sub (get_local $h) (get_local $th))
          )
          (get_local  $img)
          (i32.const 0)
          (i32.const 0)
          (get_local  $w)
          (get_local  $h)
        )
        (set_global $~txtY (i32.sub (get_local $h) (get_local $th)))
        (call $~rect
          (get_local $img)
          (i32.const 0)
          (get_global $~txtY)
          (get_local $w)
          (get_local $h)
          (call $~pget
            (get_local $img)
            (i32.sub (get_local $w) (i32.const 1))
            (i32.sub (get_local $h) (i32.const 1))
          )
        )
        (set_global $~blending_mode (get_local $x))
        (set_global $~opacity (get_local $y))
      ))
    ))
    (set_local $len (i32.sub (get_local $len) (call $-char_size (i32.load8_u (get_local $pos)))))
    (set_local $pos (i32.add (get_local $pos) (call $-char_size (i32.load8_u (get_local $pos)))))
  (br 0)))
)
