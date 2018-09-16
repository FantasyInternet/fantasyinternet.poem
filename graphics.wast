(global $_data (mut i32) (i32.const 0))
(global $_width (mut i32) (i32.const 0))
(global $_height (mut i32) (i32.const 0))
(global $screen (mut i32) (i32.const 0))

(func $create_image (param $width i32) (param $height i32) (result i32)
  (local $out i32)
  (if (i32.eqz (get_global $_data))(then
    (set_global $_data (call $-new_value   (i32.const 3) (i32.const 4)))
    (call $-write32 (get_global $_data)    (i32.const 0) (i32.const 0x61746164))
    (set_global $_width (call $-new_value  (i32.const 3) (i32.const 5)))
    (call $-write32 (get_global $_width)   (i32.const 0) (i32.const 0x74646977))
    (call $-write8  (get_global $_width)   (i32.const 4) (i32.const 0x68))
    (set_global $_height (call $-new_value (i32.const 3) (i32.const 6)))
    (call $-write32 (get_global $_height)  (i32.const 0) (i32.const 0x67696568))
    (call $-write16 (get_global $_height)  (i32.const 4) (i32.const 0x7468))
  ))
  (set_local $out (call $-new_value (i32.const 5) (i32.const 0)))
  (call $-set_to_obj
    (get_local $out)
    (get_global $_data)
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
  (call $-set_to_obj (get_local $out) (get_global $_width) (get_local $width))
  (call $-set_to_obj (get_local $out) (get_global $_height) (get_local $height))
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
  (set_local $w (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $_width))))
  (set_local $h (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $_height))))
  (set_local $img (call $-get_from_obj (get_local $img) (get_global $_data)))
  (if
    (i32.and
      (i32.lt_u (get_local $x) (get_local $w))
      (i32.lt_u (get_local $y) (get_local $h))
    )
  (then
    (call $-write32
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
      (get_local $c)
    )
  ))
  (i32.const 0)
)
(func $pget (param $img i32) (param $x i32) (param $y i32) (result i32)
  (local $w i32)
  (local $h i32)
  (local $c i32)
  (if (i32.eq (call $-datatype (get_local $img)) (i32.const 2))(then
    (set_local $y (get_local $x))
    (set_local $x (get_local $img))
    (set_local $img (get_global $screen))
  ))
  (set_local $x (call $-i32_u (get_local $x)))
  (set_local $y (call $-i32_u (get_local $y)))
  (set_local $w (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $_width))))
  (set_local $h (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $_height))))
  (set_local $img (call $-get_from_obj (get_local $img) (get_global $_data)))
  (set_local $c (call $-new_value (i32.const 6) (i32.const 4)))
  (if
    (i32.and
      (i32.lt_u (get_local $x) (get_local $w))
      (i32.lt_u (get_local $y) (get_local $h))
    )
  (then
    (call $-write32
      (get_local $c)
      (i32.const 0)
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
  (local $i i32)
  (local $j i32)
  (local $imgOffset i32)
  (local $imgWidth i32)
  (local $imgHeight i32)
  (i32.const 0)
  (if (i32.eq (call $-datatype (get_local $img)) (i32.const 2))(then
    (set_local $c (get_local $h))
    (set_local $h (get_local $w))
    (set_local $w (get_local $y))
    (set_local $y (get_local $x))
    (set_local $x (get_local $img))
    (set_local $img (get_global $screen))
  ))
  (set_local $x (call $-i32_s (get_local $x)))
  (set_local $y (call $-i32_s (get_local $y)))
  (set_local $w (call $-i32_s (get_local $w)))
  (set_local $h (call $-i32_s (get_local $h)))
  (set_local $c (call $-read32 (get_local $c) (i32.const 0)))

  (set_local $imgWidth  (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $_width ))))
  (set_local $imgHeight (call $-i32_u (call $-get_from_obj (get_local $img) (get_global $_height))))
  (set_local $img       (call $-get_from_obj (get_local $img) (get_global $_data)))
  (set_local $imgOffset (call $-offset (get_local $img)))
  
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
  (block (loop
    (br_if 1 (i32.eq (get_local $h) (i32.const 0)))
    (set_local $j (get_local $w))
    (block (loop
      (br_if 1 (i32.eq (get_local $j) (i32.const 0)))
      (i32.store (i32.add (get_local $imgOffset) (get_local $i)) (get_local $c))
      (set_local $i (i32.add (get_local $i) (i32.const 4)))
      (set_local $j (i32.sub (get_local $j) (i32.const 1)))
      (br 0)
    ))
    (set_local $i (i32.sub (i32.add (get_local $i) (i32.mul (i32.const 4) (get_local $imgWidth))) (i32.mul (i32.const 4) (get_local $w))))
    (set_local $h (i32.sub (get_local $h) (i32.const 1)))
    (br 0)
  ))
)
